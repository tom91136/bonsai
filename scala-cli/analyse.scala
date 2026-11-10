#!/usr/bin/env -S scala-cli shebang --server=false
//> using scala 3.8
// Runs the whole-build analysis over a Scala CLI project on the JVM: compiles it with its tests, writes the manifest the
// sbt plugin would, and runs the analysis with the options of its .bonsai.conf, then the remaining arguments, such as
// --mode strict.
// The analysis needs JDK 17 or later and runs on BONSAI_ANALYSIS_JAVA_HOME, else on the java on the PATH, which is also
// the JDK the manifest names as the target.
// usage: analyse.scala <project directory> [--library] [analysis options...]

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*

val usage = "usage: analyse.scala <project directory> [--library] [analysis options...]"

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
    dir: Option[Path],
    env: Seq[(String, String)],
    stdout: ProcessBuilder.Redirect,
    merge: Boolean = false
): Process = {
  val builder = new ProcessBuilder(args.asJava).redirectErrorStream(merge)
  dir.foreach(d => builder.directory(d.toFile))
  env.foreach((k, v) => builder.environment().put(k, v))
  builder.redirectInput(ProcessBuilder.Redirect.INHERIT).redirectOutput(stdout)
  if (!merge) builder.redirectError(ProcessBuilder.Redirect.INHERIT)
  try builder.start()
  catch { case _: java.io.IOException => fail(127, s"${args.head}: command not found") }
}

def capture(args: Seq[String], dir: Option[Path], merge: Boolean = false): String = {
  val process = start(args, dir, Seq.empty, ProcessBuilder.Redirect.PIPE, merge)
  val text    = new String(process.getInputStream.readAllBytes(), ISO_8859_1)
  val code    = process.waitFor()
  if (code != 0) sys.exit(code)
  text.replaceAll("\n+$", "")
}

def unzipEntry(jar: String, entry: String): String = {
  if (!Files.exists(Path.of(jar))) fail(9, s"unzip: cannot find or open $jar")
  val zip = new ZipFile(jar)
  try
    Option(zip.getEntry(entry)) match {
      case Some(e) => new String(zip.getInputStream(e).readAllBytes(), ISO_8859_1).replaceAll("\n+$", "")
      case None    => fail(11, s"unzip: filename not matched: $entry")
    }
  finally zip.close()
}

@main def analyse(args: String*): Unit = {
  val directory = args.headOption.filter(_.nonEmpty).getOrElse(fail(1, usage))
  val project   = Path.of(directory).toAbsolutePath.normalize
  if (!Files.isDirectory(project)) fail(1, s"cd: $directory: No such file or directory")
  var rest    = args.tail
  var library = false
  if (rest.headOption.contains("--library")) {
    library = true
    rest = rest.tail
  }
  val name = project.getFileName.toString

  val properties = capture(Seq("java", "-XshowSettings:properties", "-version"), None, merge = true)
  val javaHome   = properties.linesIterator
    .map(_.trim.split("\\s+"))
    .collectFirst { case Array("java.home", _, home, _*) => home }
    .getOrElse("")
  val configured   = sys.env.get("BONSAI_ANALYSIS_JAVA_HOME").filter(_.nonEmpty)
  val analysisHome = configured.getOrElse(javaHome)
  val versionLine  =
    capture(Seq(s"$analysisHome/bin/java", "-version"), None, merge = true).linesIterator.find(_.contains("version \""))
  val analysisMajor = versionLine.flatMap { line =>
    val parts   = line.split("\"")(1).split("\\.")
    val numeric = parts.map(_.takeWhile(_.isDigit).toIntOption.getOrElse(0))
    if (numeric(0) == 1) numeric.lift(1) else Some(numeric(0))
  }
  if (analysisMajor.getOrElse(0) < 17) {
    val source = if (configured.isDefined) "BONSAI_ANALYSIS_JAVA_HOME" else "the java on the PATH"
    fail(
      1,
      s"bonsai: the analysis needs JDK 17 or later but $source is $analysisHome, " +
        s"JDK ${analysisMajor.fold("unknown")(_.toString)}; set BONSAI_ANALYSIS_JAVA_HOME to a JDK 17 or later"
    )
  }
  val classpath =
    capture(Seq("scala-cli", "compile", ".", "--test", "--platform", "jvm", "--print-class-path"), Some(project))

  val ivy      = sys.env.get("IVY_HOME").filter(_.nonEmpty).getOrElse(s"${sys.env("HOME")}/.ivy2/local")
  val analysis = s"$ivy/net.kurobako/bonsai-analysis_3/0.1.0-SNAPSHOT/jars/bonsai-analysis_3.jar"
  val scala    = unzipEntry(analysis, "META-INF/bonsai/scala.id")
  val runner   = Files.createTempDirectory("tmp")
  Runtime.getRuntime.addShutdownHook(new Thread(() => removeAll(runner)))

  def tool(main: String, arguments: Seq[String]): Int = {
    val command = Seq("scala-cli", "run", "--server=false", "--platform", "jvm", "--jvm", "system", "-S", scala) ++
      Seq("--dep", "net.kurobako:bonsai-analysis_3:0.1.0-SNAPSHOT", "--java-opt", "-Xss1g", "--java-opt", "-Xmx4g") ++
      Seq("-M", main, "--") ++ arguments
    start(command, Some(runner), Seq("JAVA_HOME" -> analysisHome), ProcessBuilder.Redirect.INHERIT).waitFor()
  }

  val configuration = runner.resolve("configuration.tsv")
  val status        = tool(
    "net.kurobako.bonsai.analysis.Configure",
    Seq("--config", s"$project/.bonsai.conf", "--module", name, "--out", configuration.toString)
  )
  if (status != 0) sys.exit(status)
  val manifest = project.resolve(".scala-build/bonsai/manifest.tsv")
  Files.createDirectories(manifest.getParent)
  val settings =
    if (Files.exists(configuration)) Files.readAllLines(configuration, ISO_8859_1).asScala.toVector
    else fail(2, s"awk: cannot open $configuration")
  if (settings.contains("library\ttrue")) library = true
  val entries = classpath.split(":", -1).toVector.map {
    case entry if entry.endsWith("/classes/main") => s"output\t$name\tmain\t$entry"
    case entry if entry.endsWith("/classes/test") => s"output\t$name\ttest\t$entry"
    case entry                                    => s"classpath\t$name\t$entry"
  }
  val lines = Vector(s"root\t$project", s"jdk\t$javaHome") ++ entries ++
    (if (library) Vector(s"library\t$name") else Vector.empty) ++
    settings.filter(line => Set("text", "text-exclude")(line.split("\t", -1)(0)))
  Files.writeString(manifest, lines.map(_ + "\n").mkString, ISO_8859_1)
  val options = settings.map(_.split("\t", -1)).filter(_(0) == "option").map(_.lift(1).getOrElse(""))
  sys.exit(tool("net.kurobako.bonsai.analysis.CLI", Seq("--manifest", manifest.toString) ++ options ++ rest))
}
