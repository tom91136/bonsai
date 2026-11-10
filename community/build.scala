#!/usr/bin/env -S scala-cli shebang --server=false
//> using scala 3.8
// Runs inside the community image: clones one project, injects bonsai, runs sbt and writes the outputs to /out.
// usage: build.scala <sbt command>...
// environment: PROJECT_URL PROJECT_REF JAVA_MAJOR TIMEOUT_SECONDS BONSAI_VERSION, and optionally PROJECT_TAG and BONSAI_INJECT
// BONSAI_INJECT=no builds the project untouched, as the control of a run.
// PROJECT_TAG is a tag to fetch beside the ref, or ALL to unshallow the clone with every tag, for builds that read git tags.
// sbt runs on JAVA_MAJOR, which the build targets; the analysis runs on JDK 25.

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.nio.file.StandardOpenOption
import java.time.Instant
import java.util.Locale
import java.util.concurrent.atomic.AtomicBoolean
import scala.jdk.CollectionConverters.*
import scala.util.Try

def required(name: String): String = sys.env.get(name).filter(_.nonEmpty).getOrElse {
  System.err.println(s"build.scala: $name: parameter null or not set")
  sys.exit(1)
}

val out         = Path.of("/out")
val src         = Path.of("/work/src")
val url         = required("PROJECT_URL")
val ref         = required("PROJECT_REF")
val major       = required("JAVA_MAJOR")
val limit       = required("TIMEOUT_SECONDS")
val version     = required("BONSAI_VERSION")
val tag         = sys.env.getOrElse("PROJECT_TAG", "")
val javaHome    = s"/opt/jdk/$major"
val analysisJdk = "/opt/jdk/25"
val env         = Seq("JAVA_HOME" -> javaHome, "PATH" -> s"$javaHome/bin:${sys.env.getOrElse("PATH", "")}")

def now(): Double = System.nanoTime() / 1e9

def elapsed(a: Double, b: Double): String = "%.1f".formatLocal(Locale.ROOT, b - a)

def write(file: Path, text: String): Unit = Files.writeString(file, text, ISO_8859_1)

def append(file: Path, text: String): Unit =
  Files.writeString(file, text, ISO_8859_1, StandardOpenOption.CREATE, StandardOpenOption.APPEND)

def command(args: Seq[String], stdout: ProcessBuilder.Redirect, stderr: ProcessBuilder.Redirect): Process = {
  val builder = new ProcessBuilder(args.asJava).directory(src.toFile)
  env.foreach((k, v) => builder.environment().put(k, v))
  builder.redirectInput(ProcessBuilder.Redirect.INHERIT).redirectOutput(stdout).redirectError(stderr).start()
}

def to(file: Path): ProcessBuilder.Redirect = ProcessBuilder.Redirect.to(file.toFile)

def appendTo(file: Path): ProcessBuilder.Redirect = ProcessBuilder.Redirect.appendTo(file.toFile)

def exec(args: Seq[String], stdout: ProcessBuilder.Redirect, stderr: ProcessBuilder.Redirect): Int = {
  val process = command(args, stdout, stderr)
  process.waitFor()
}

def capture(args: Seq[String], stderr: ProcessBuilder.Redirect): (Int, Array[Byte]) = {
  val process = command(args, ProcessBuilder.Redirect.PIPE, stderr)
  val bytes   = process.getInputStream.readAllBytes()
  (process.waitFor(), bytes)
}

def inject(): Unit =
  if (!sys.env.get("BONSAI_INJECT").contains("no")) {
    Files.createDirectories(src.resolve("project"))
    write(
      src.resolve("project/bonsai-community.sbt"),
      s"""addSbtPlugin("net.kurobako" % "sbt-bonsai" % "$version")\n"""
    )
    write(src.resolve("bonsai-community.sbt"), s"""ThisBuild / bonsaiAnalysisJavaHome := file("$analysisJdk")\n""")
  }

def stamp(log: Path, stamped: Path, stop: AtomicBoolean): Unit = {
  val sink             = Files.newOutputStream(stamped)
  val input            = Files.newInputStream(log)
  val buffer           = new java.io.ByteArrayOutputStream()
  val chunk            = new Array[Byte](65536)
  def drain(): Boolean = {
    val n = input.read(chunk)
    if (n <= 0) false
    else {
      for (i <- 0 until n)
        if (chunk(i) == '\n') {
          val seconds = Instant.now()
          sink.write(f"${seconds.getEpochSecond}.${seconds.getNano / 1000}%06d ".getBytes(ISO_8859_1))
          buffer.writeTo(sink)
          sink.write('\n')
          buffer.reset()
        } else buffer.write(chunk(i).toInt)
      true
    }
  }
  try {
    while (!stop.get()) if (!drain()) Thread.sleep(50)
    while (drain()) ()
  } finally {
    input.close()
    sink.close()
  }
}

def manifestName(file: Path): String =
  file.toString.replaceFirst("^/work/src/", "").replace("/", "__").replaceFirst("^\\.", "_")

def peakKb(pid: Long): Long =
  Try {
    Files.readAllLines(Path.of(s"/proc/$pid/status"), ISO_8859_1).asScala
      .find(_.startsWith("VmHWM"))
      .map(_.split("\\s+")(1).toLong)
      .getOrElse(0L)
  }.getOrElse(0L)

def samplePeak(process: Process): Long = {
  var peak = 0L
  while (process.isAlive) {
    peak = peak.max(peakKb(process.pid))
    process.waitFor(1, java.util.concurrent.TimeUnit.SECONDS)
  }
  peak
}

def readOr(file: String, default: String): String = Try(Files.readString(Path.of(file), ISO_8859_1)).getOrElse(default)

@main def build(sbt: String*): Unit = {
  Files.createDirectories(out)
  val timings = out.resolve("timings.tsv")
  write(timings, "")
  val t0 = now()
  Files.createDirectories(src)
  val cloneLog = out.resolve("clone.log")
  val devNull  = ProcessBuilder.Redirect.DISCARD
  val inherit  = ProcessBuilder.Redirect.INHERIT
  exec(Seq("git", "init", "-q"), inherit, inherit)
  exec(Seq("git", "remote", "add", "origin", url), inherit, inherit)
  val cloned =
    exec(Seq("git", "-c", "safe.directory=*", "fetch", "-q", "--depth", "1", "origin", ref), inherit, to(cloneLog)) ==
      0 &&
      exec(Seq("git", "checkout", "-q", "FETCH_HEAD"), inherit, appendTo(cloneLog)) == 0
  if (!cloned) {
    write(out.resolve("status"), "clone-failed\n")
    sys.exit(0)
  }
  val commit = new String(capture(Seq("git", "rev-parse", "HEAD"), inherit)._2, ISO_8859_1).stripTrailing()
  if (tag == "ALL") exec(Seq("git", "fetch", "-q", "--unshallow", "--tags", "origin"), inherit, appendTo(cloneLog))
  else if (tag.nonEmpty)
    exec(
      Seq("git", "fetch", "-q", "--depth", "1", "origin", s"refs/tags/$tag:refs/tags/$tag"),
      inherit,
      appendTo(cloneLog)
    )
  val t1 = now()
  append(timings, s"clone\t${elapsed(t0, t1)}\n")

  inject()

  val log  = out.resolve("build.log")
  val stop = new AtomicBoolean(false)
  write(log, "")
  val stamper = new Thread(() => stamp(log, out.resolve("build.ts.log"), stop))
  stamper.start()
  val code = exec(Seq("timeout", "-k", "30", limit, "sbt", "--server", "--batch") ++ sbt, appendTo(log), appendTo(log))
  val t2   = now()
  stop.set(true)
  stamper.join()
  append(timings, s"build\t${elapsed(t1, t2)}\n")

  val oom = readOr("/sys/fs/cgroup/memory.events", "0").linesIterator
    .collectFirst { case l if l.startsWith("oom_kill ") => l.stripPrefix("oom_kill ") }
    .flatMap(_.toLongOption)
    .getOrElse(0L)
  val status = code match {
    case _ if code != 124 && oom > 0 => "oom"
    case 0                           => "ok"
    case 124                         => "timeout"
    case 137                         => "killed"
    case _                           => "failed"
  }
  write(out.resolve("status"), s"$status\n")

  write(
    out.resolve("meta.tsv"),
    s"""url\t$url
       |ref\t$ref
       |commit\t$commit
       |java\t$major
       |bonsai\t$version
       |sbt\t${sbt.mkString(" ")}
       |exit\t$code
       |oom_kill\t$oom
       |""".stripMargin
  )
  write(out.resolve("peak-memory-bytes"), readOr("/sys/fs/cgroup/memory.peak", ""))

  Files.createDirectories(out.resolve("manifests"))
  Files.createDirectories(out.resolve("info"))
  val (_, found) = capture(
    Seq("find", src.toString, "-path", "*/target/*", "-path", "*/bonsai/manifest.tsv", "-print0"),
    ProcessBuilder.Redirect.INHERIT
  )
  var toolCp = ""
  new String(found, ISO_8859_1).split('\u0000').filter(_.nonEmpty).foreach { name =>
    val file = Path.of(name)
    val id   = manifestName(file)
    Files.copy(file, out.resolve("manifests").resolve(id), StandardCopyOption.REPLACE_EXISTING)
    if (toolCp.isEmpty) {
      val coordinate = s"net.kurobako:bonsai-analysis_3:$version"
      toolCp = new String(
        capture(Seq("cs", "fetch", "--classpath", "-r", "ivy2Local", coordinate), to(out.resolve("info/cs.err")))._2,
        ISO_8859_1
      ).replaceAll("\n+$", "")
    }
    if (toolCp.nonEmpty) {
      val started = now()
      val process = command(
        Seq(
          s"$analysisJdk/bin/java",
          "-Xss1g",
          "-Xmx6g",
          "-cp",
          toolCp,
          "net.kurobako.bonsai.analysis.CLI",
          "--manifest",
          name,
          "--mode",
          "relaxed",
          "--info"
        ),
        to(out.resolve(s"info/$id.out")),
        to(out.resolve(s"info/$id.err"))
      )
      val peak = samplePeak(process)
      val exit = process.waitFor()
      append(out.resolve("info/runs.tsv"), s"$id\t$exit\t${elapsed(started, now())}\tpeak_kb=$peak\n")
    }
  }

  val reportable = "^(error|warning|info)\t".r
  val lines      = Files.lines(log, ISO_8859_1)
  try {
    val report = lines.iterator.asScala.filter(l => reportable.findFirstIn(l).isDefined || l.contains("[bonsai:"))
    write(out.resolve("report.txt"), report.map(_ + "\n").mkString)
  } finally lines.close()

  if (status != "ok") {
    val crash = out.resolve("crash.txt")
    write(crash, "")
    val (_, hs) = capture(
      Seq("find", "/", "-xdev", "-name", "hs_err_pid*.log", "-newer", cloneLog.toString, "-print"),
      devNull
    )
    new String(hs, ISO_8859_1).linesIterator.foreach { file =>
      append(crash, s"== $file\n")
      exec(Seq("cat", file), appendTo(crash), inherit)
    }
    append(crash, "== tail of build.log\n")
    exec(Seq("tail", "-n", "200", log.toString), appendTo(crash), inherit)
  }
}
