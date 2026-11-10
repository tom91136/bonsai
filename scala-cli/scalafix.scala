#!/usr/bin/env -S scala-cli shebang --server=false
//> using scala 3.8
// Runs the bonsai scalafix rules over a Scala CLI project, rewriting its sources, or only reporting them with --check.
// The project's .scalafix.conf is used when it has one, otherwise every rule the rules artefact registers is enabled,
// with the rules, bans and settings it publishes for scalafix's own syntax and unused code. Opt-in rules are left out.
// It fails when the rules artefact registers no rules or the configuration in use requests none, which scalafix itself
// passes without running anything.
// usage: scalafix.scala <project directory> [--check]

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*
import scala.util.Try

val usage = "usage: scalafix.scala <project directory> [--check]"

def fail(code: Int, message: String): Nothing = {
  System.err.println(message)
  sys.exit(code)
}

def removeAll(root: Path): Unit =
  Files.walkFileTree(
    root,
    new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes) = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }
      override def postVisitDirectory(dir: Path, error: java.io.IOException) = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    }
  )

def start(
    args: Seq[String],
    stdout: ProcessBuilder.Redirect,
    stderr: ProcessBuilder.Redirect,
    merge: Boolean = false
): Process = {
  val builder = new ProcessBuilder(args.asJava)
    .redirectInput(ProcessBuilder.Redirect.INHERIT)
    .redirectOutput(stdout)
    .redirectErrorStream(merge)
  if (!merge) builder.redirectError(stderr)
  try builder.start()
  catch { case _: java.io.IOException => fail(127, s"${args.head}: command not found") }
}

def capture(args: Seq[String], stderr: ProcessBuilder.Redirect = ProcessBuilder.Redirect.INHERIT): (Int, String) = {
  val process = start(args, ProcessBuilder.Redirect.PIPE, stderr)
  val text    = new String(process.getInputStream.readAllBytes(), ISO_8859_1)
  (process.waitFor(), text.replaceAll("\n+$", ""))
}

def unzipEntry(jar: String, entry: String): String = {
  val zip = new ZipFile(jar)
  try
    Option(zip.getEntry(entry)) match {
      case Some(e) => new String(zip.getInputStream(e).readAllBytes(), ISO_8859_1).replaceAll("\n+$", "")
      case None    => fail(11, s"unzip: filename not matched: $entry")
    }
  finally zip.close()
}

def leadingNumber(text: String): Int = text.takeWhile(_.isDigit).toIntOption.getOrElse(0)

def mentionsUnused(project: Path): Boolean = {
  val unused = "-(Wunused|Ywarn-unused|Wall)".r
  var found  = false
  Files.walkFileTree(
    project,
    new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes) = {
        val name = file.getFileName.toString
        if (attrs.isRegularFile && (name.endsWith(".scala") || name.endsWith(".sc"))) {
          found =
            Try(Files.readAllLines(file, ISO_8859_1).asScala.exists(unused.findFirstIn(_).isDefined)).getOrElse(false)
        }
        if (found) FileVisitResult.TERMINATE else FileVisitResult.CONTINUE
      }
      override def visitFileFailed(file: Path, error: java.io.IOException) = FileVisitResult.CONTINUE
    }
  )
  found
}

@main def scalafix(args: String*): Unit = {
  val directory = args.headOption.filter(_.nonEmpty).getOrElse(fail(1, usage))
  val project   = Path.of(directory).toAbsolutePath.normalize
  if (!Files.isDirectory(project)) fail(1, s"cd: $directory: No such file or directory")
  val check = args.lift(1).contains("--check")
  if (args.length - 1 - (if (check) 1 else 0) != 0) fail(2, usage)

  val work = Files.createTempDirectory("tmp")
  Runtime.getRuntime.addShutdownHook(new Thread(() => removeAll(work)))
  Files.writeString(
    work.resolve("Resolve.scala"),
    "//> using dep net.kurobako:bonsai-scalafix_3:0.1.0-SNAPSHOT\nobject Resolve\n",
    ISO_8859_1
  )
  val (compiled, classpath) =
    capture(Seq("scala-cli", "compile", work.toString, "--server=false", "--platform", "jvm", "--print-class-path"))
  if (compiled != 0) sys.exit(compiled)
  val jars  = classpath.split(":", -1).toVector
  val rules = jars.find(_.matches(".*/bonsai-scalafix_3(-[^/]*)?\\.jar")).getOrElse(sys.exit(1))
  val tool  = Some(jars.filter(_.matches(".*/(bonsai-scalafix_3|scala3-library_3|scala-library)(-[^/]*)?\\.jar")))
    .filter(_.nonEmpty)
    .getOrElse(sys.exit(1))
    .mkString(":")
  def published(name: String): String = unzipEntry(rules, s"META-INF/bonsai/$name")
  def property(key: String): String   =
    published("versions.properties").linesIterator.collectFirst {
      case l if l.startsWith(s"$key=") => l.stripPrefix(s"$key=")
    }
      .getOrElse("")

  val scala = {
    val (_, output) = capture(
      Seq("scala-cli", "compile", project.toString, "--server=false", "--print-class-path"),
      ProcessBuilder.Redirect.DISCARD
    )
    output.split(":", -1).collectFirst {
      case jar if jar.matches(".*/scala3-library_3-[^/]*\\.jar") =>
        jar.replaceFirst(".*/scala3-library_3-([^/]*)\\.jar$", "$1")
    }.getOrElse("")
  }
  val version        = "^3\\.([0-9]+)\\.([0-9]+).*".r
  val (minor, patch) = scala match {
    case version(m, p) => (Some(m.toInt), p.toInt)
    case _             => (None, 0)
  }
  def supported(rule: String): Boolean = minor.forall { m =>
    rule match {
      case "RemoveUnused" =>
        val from      = property("removeUnused.from")
        val fromMinor = leadingNumber(from.stripPrefix("3."))
        val fromPatch = leadingNumber(from.substring(from.lastIndexOf('.') + 1))
        m > fromMinor || (m == fromMinor && patch >= fromPatch)
      case _ => true
    }
  }

  val builtin = published("builtin.id").linesIterator.filterNot(_.startsWith("?")).toVector
  if (builtin.isEmpty) sys.exit(1)
  val registered = unzipEntry(rules, "META-INF/services/scalafix.v1.Rule").linesIterator
    .collect { case line if line.matches(".*\\.[^.]+") => "  " + line.substring(line.lastIndexOf('.') + 1) }
    .toVector
  if (registered.isEmpty) fail(1, s"scalafix.scala: $rules registers no rules")
  val declared = builtin.filterNot(_.startsWith("-"))
  val conf     = Vector("rules = [") ++ registered ++
    declared.filter(line => !line.contains("=") && !line.contains(".")).map(_.trim).filter(supported).map("  " + _) ++
    Vector("]") ++
    declared.filter(_.contains("=")) ++
    declared.filter(line => !line.contains("=") && line.contains(".")).map(_ + " = true")
  Files.writeString(work.resolve(".scalafix.conf"), conf.map(_ + "\n").mkString, ISO_8859_1)

  var scalac = Vector.empty[String]
  if (supported("RemoveUnused") && !mentionsUnused(project)) {
    scalac = builtin.filter(line => line.startsWith("-") && !line.startsWith("-Wconf")).map(o => s"--scalac-option=$o")
  }
  var config = work.resolve(".scalafix.conf")
  if (Files.isRegularFile(project.resolve(".scalafix.conf"))) {
    config = project.resolve(".scalafix.conf")
    scalac = Vector.empty
  }

  val fix = Seq(
    "scala-cli",
    "fix",
    "--power",
    "--enable-built-in=false",
    s"--scalafix-conf=$config",
    s"--scalafix-arg=--tool-classpath=$tool"
  ) ++
    scalac ++ (if (check) Seq("--check") else Seq.empty) ++ Seq(project.toString)
  val log     = work.resolve("fix.log")
  val process = start(fix, ProcessBuilder.Redirect.PIPE, ProcessBuilder.Redirect.INHERIT, merge = true)
  val sink    = Files.newOutputStream(log)
  val buffer  = new Array[Byte](8192)
  val input   = process.getInputStream
  var n       = input.read(buffer)
  while (n >= 0) {
    System.out.write(buffer, 0, n)
    System.out.flush()
    sink.write(buffer, 0, n)
    n = input.read(buffer)
  }
  sink.close()
  val status = process.waitFor()
  if (Files.readAllLines(log, ISO_8859_1).asScala.exists(_.contains("No rules requested to run"))) {
    fail(1, s"scalafix.scala: no rules ran; $config requests none")
  }
  sys.exit(status)
}
