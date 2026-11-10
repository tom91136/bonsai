#!/usr/bin/env -S scala-cli shebang --server=false
//> using scala 3.8
// Runs the working tree's analysis over a reference build's manifest and, given a copy of that build, its compile
// checks, then compares the findings of each rule with the previous run over the same build and lists the rules whose
// count jumped; the findings of the last run stay beside the counts. The copy is compiled against a snapshot published
// aside, so ~/.ivy2/local is left alone; never pass the build's own working tree, which the compile writes to.
// usage: corpus.scala <manifest> [<copy of the build directory>]

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.StandardCopyOption
import java.nio.file.attribute.BasicFileAttributes
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

val usage = "usage: corpus.scala <manifest> [<copy of the build directory>]"

def fail(code: Int, message: String): Nothing = {
  System.err.println(message)
  sys.exit(code)
}

def realpath(path: String): Path = {
  val absolute = Path.of(path).toAbsolutePath.normalize
  try
    if (Files.exists(absolute)) absolute.toRealPath() else absolute.getParent.toRealPath().resolve(absolute.getFileName)
  catch { case _: java.io.IOException => fail(1, s"realpath: $path: No such file or directory") }
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

def exec(
    args: Seq[String],
    dir: Path,
    stdout: ProcessBuilder.Redirect = ProcessBuilder.Redirect.INHERIT,
    stderr: ProcessBuilder.Redirect = ProcessBuilder.Redirect.INHERIT
): Int =
  new ProcessBuilder(args.asJava)
    .directory(dir.toFile)
    .redirectInput(ProcessBuilder.Redirect.INHERIT)
    .redirectOutput(stdout)
    .redirectError(stderr)
    .start()
    .waitFor()

def check(code: Int): Unit = if (code != 0) sys.exit(code)

def to(file: Path): ProcessBuilder.Redirect = ProcessBuilder.Redirect.to(file.toFile)

def counts(rules: Seq[String]): Seq[(String, Long)] =
  rules.groupBy(identity).toSeq.map((rule, same) => rule -> same.size.toLong).sortBy(_._1)

def parse(lines: Seq[String]): Seq[(String, Long)] = lines.map { line =>
  val fields = line.split("\t", -1)
  fields(0) -> fields.lift(1).flatMap(_.toLongOption).getOrElse(0L)
}

@main def corpus(args: String*): Unit = {
  val manifest = realpath(args.headOption.filter(_.nonEmpty).getOrElse(fail(1, usage)))
  val copy     = args.lift(1).filter(_.nonEmpty).map(realpath)
  val root     = Path.of(sys.props("scala.sources")).toAbsolutePath.getParent.getParent
  val hash     =
    MessageDigest.getInstance("SHA-1").digest(s"$manifest\n".getBytes(ISO_8859_1)).map("%02x".format(_)).mkString
  val history = Path.of(sys.env("HOME"), ".cache/bonsai-corpus", hash.take(12))
  Files.createDirectories(history)
  val stage = Files.createTempDirectory("tmp")
  Runtime.getRuntime.addShutdownHook(new Thread(() => removeAll(stage)))
  val ivy     = Seq(s"-Dsbt.ivy.home=$stage", s"-Divy.home=$stage")
  val devNull = ProcessBuilder.Redirect.DISCARD

  exec(Seq("sbt", "--client", "shutdown"), root, devNull, devNull)
  // one heavy JVM at a time: the host is shared with other builds
  val classpathRun = new ProcessBuilder((Seq("sbt", "--batch") ++ ivy :+ "analysis/analysisClasspath").asJava)
    .directory(root.toFile)
    .redirectInput(ProcessBuilder.Redirect.INHERIT)
    .redirectError(ProcessBuilder.Redirect.INHERIT)
    .start()
  val printed = new String(
    classpathRun.getInputStream.readAllBytes(),
    ISO_8859_1
  ).linesIterator.filterNot(_.startsWith("[")).toVector
  val sbtCode = classpathRun.waitFor()
  if (printed.isEmpty) sys.exit(1)
  check(sbtCode)
  val classpath = printed.last
  Files.writeString(stage.resolve("classpath"), classpath + "\n", ISO_8859_1)

  val analysis = stage.resolve("analysis.txt")
  val problems = stage.resolve("analysis.err")
  exec(
    Seq(
      "java",
      "-Xss1g",
      "-Xmx4g",
      "-cp",
      classpath,
      "net.kurobako.bonsai.analysis.CLI",
      "--manifest",
      manifest.toString,
      "--mode",
      "relaxed",
      "--info"
    ),
    root,
    to(analysis),
    to(problems)
  )
  var found =
    counts(Files.readAllLines(analysis, ISO_8859_1).asScala.toSeq.map(_.split("\t", -1).lift(1).getOrElse("")))
  copy.foreach { directory =>
    check(exec(Seq("sbt", "--batch") ++ ivy :+ "publishLocalAll", root, devNull))
    val compile = stage.resolve("compile.txt")
    exec(
      Seq("sbt", "--batch") ++ ivy ++ Seq(
        """set Global / localCacheDirectory := file(sys.props("java.io.tmpdir") + "/bonsai-corpus-cache")""",
        "Test/compile"
      ),
      directory,
      to(compile),
      ProcessBuilder.Redirect.appendTo(compile.toFile)
    )
    val pattern  = "\\[bonsai:([a-z-]*)\\]".r
    val compiled =
      Files.readAllLines(compile, ISO_8859_1).asScala.toSeq.flatMap(pattern.findAllMatchIn(_).map(_.group(1)))
    found ++= counts(compiled)
  }
  val lines = found.map((rule, count) => s"$rule\t$count")
  Files.writeString(stage.resolve("counts.tsv"), lines.map(_ + "\n").mkString, ISO_8859_1)
  Files.readAllLines(problems, ISO_8859_1).asScala.filter(_.startsWith("problem:")).foreach(println)

  val previous = history.resolve("counts.tsv")
  val before = if (Files.exists(previous)) parse(Files.readAllLines(previous, ISO_8859_1).asScala.toSeq) else Seq.empty
  println("%-32s %8s %8s".format("rule", "before", "after"))
  (before.map(_._1) ++ found.map(_._1)).distinct.sorted.foreach { rule =>
    val earlier = before.filter(_._1 == rule).map(_._2)
    val later   = found.filter(_._1 == rule).map(_._2)
    val pairs   = if (earlier.isEmpty) later.map(0L -> _)
    else if (later.isEmpty) earlier.map(_ -> 0L)
    else for {
      b <- earlier
      a <- later
    } yield b -> a
    pairs.foreach { (b, a) =>
      val change = a - b
      val mark   = if (math.abs(change) > 20 && math.abs(change) * 2 > b) " <- jump" else ""
      println("%-32s %8d %8d%s".format(rule, b, a, mark))
    }
  }
  Files.copy(stage.resolve("counts.tsv"), previous, StandardCopyOption.REPLACE_EXISTING)
  Files.copy(analysis, history.resolve("analysis.txt"), StandardCopyOption.REPLACE_EXISTING)
}
