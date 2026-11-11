#!/usr/bin/env -S scala-cli shebang --server=false
//> using scala 3.8
// Builds one project with bonsai in a fresh container and writes its outputs to <out>/<name>.
// usage: run.scala --url <git url or local repository> --ref <ref> --java <major> --ivy <ivy home> [options] [--sbt <command>]...
//   --out DIR        output root (default community-out)
//   --name NAME      output directory name (default derived from the url)
//   --image IMAGE    image to run (default localhost/bonsai-community:latest)
//   --memory SIZE    container memory cap (default 12g)
//   --memory-swap SIZE  cap on memory plus swap (default: --memory, so no swap)
//   --heap SIZE      sbt heap (default 6g)
//   --cpus N         cpu cap
//   --timeout SEC    wall-clock limit for the sbt run (default 3600)
//   --cache VOLUME   named volume holding the dependency caches, kept afterwards (default: a fresh volume, removed after)
//   --version V      bonsai version in the ivy repository (default 0.1.0-SNAPSHOT)
//   --bonsai no      build the project without bonsai, as the control of a run
//   --tag TAG        a git tag to fetch beside the ref, or ALL to unshallow with every tag, for builds that read tags
// --ivy is the ivy home the bonsai artefacts were published to with -Dsbt.ivy.home; its local/ is mounted read-only.
// A build that fails, times out or crashes is a result in <out>/<name>/status, not an error of this script.

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.attribute.PosixFilePermissions
import scala.jdk.CollectionConverters.*
import scala.util.Random

def fail(code: Int, message: String): Nothing = {
  System.err.println(message)
  sys.exit(code)
}

def realpath(path: String): Path = {
  val absolute = Path.of(path).toAbsolutePath.normalize
  var existing = absolute
  while (!Files.exists(existing)) existing = existing.getParent
  existing.toRealPath().resolve(existing.relativize(absolute)).normalize
}

def derivedName(url: String): String =
  url
    .replaceFirst("/+$", "")
    .replaceFirst("\\.git$", "")
    .replaceFirst(".*[:/]([^/]+/[^/]+)$", "$1")
    .replace("/", "__")

def removeAll(root: Path): Unit =
  if (Files.exists(root, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
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
  }

def quietly(args: String*): Unit =
  try {
    new ProcessBuilder(args.asJava)
      .redirectInput(ProcessBuilder.Redirect.INHERIT)
      .redirectOutput(ProcessBuilder.Redirect.DISCARD)
      .redirectError(ProcessBuilder.Redirect.DISCARD)
      .start()
      .waitFor()
    ()
  } catch { case _: java.io.IOException => () }

@main def run(args: String*): Unit = {
  val here           = Path.of(sys.props("scala.sources")).toAbsolutePath.getParent
  val engine         = sys.env.get("CONTAINER_ENGINE").filter(_.nonEmpty).getOrElse("podman")
  var image          = "localhost/bonsai-community:latest"
  var out            = "community-out"
  var memory         = "12g"
  var memorySwap     = ""
  var heap           = "6g"
  var timeoutSeconds = "3600"
  var version        = "0.1.0-SNAPSHOT"
  var url            = ""
  var ref            = ""
  var java           = ""
  var ivy            = ""
  var name           = ""
  var cpus           = ""
  var cache          = ""
  var tag            = ""
  var inject         = "yes"
  val sbt            = Vector.newBuilder[String]

  var rest = args.toList
  while (rest.nonEmpty) {
    val option                            = rest.head
    val value                             = rest.tail.headOption
    def set(assign: String => Unit): Unit = assign(value.getOrElse(fail(1, s"$option: missing value")))
    option match {
      case "--url"         => set(url = _)
      case "--ref"         => set(ref = _)
      case "--java"        => set(java = _)
      case "--ivy"         => set(ivy = _)
      case "--out"         => set(out = _)
      case "--name"        => set(name = _)
      case "--image"       => set(image = _)
      case "--memory"      => set(memory = _)
      case "--memory-swap" => set(memorySwap = _)
      case "--heap"        => set(heap = _)
      case "--cpus"        => set(cpus = _)
      case "--timeout"     => set(timeoutSeconds = _)
      case "--cache"       => set(cache = _)
      case "--version"     => set(version = _)
      case "--tag"         => set(tag = _)
      case "--bonsai"      => set(inject = _)
      case "--sbt"         => set(sbt += _)
      case _               => fail(2, s"unknown option: $option")
    }
    rest = rest.drop(2)
  }
  if (url.isEmpty) fail(1, "--url is required")
  if (ref.isEmpty) fail(1, "--ref is required")
  if (java.isEmpty) fail(1, "--java is required")
  if (ivy.isEmpty) fail(1, "--ivy is required")
  if (!Files.isDirectory(Path.of(ivy, "local"))) fail(2, s"no $ivy/local")
  val limit    = timeoutSeconds.toLongOption.getOrElse(fail(1, s"--timeout: not a number: $timeoutSeconds"))
  val commands = sbt.result().headOption.fold(Vector("bonsaiAnalyse"))(_ => sbt.result())
  if (name.isEmpty) name = derivedName(url)

  val dir = realpath(out).resolve(name)
  removeAll(dir)
  Files.createDirectories(dir)
  Files.setPosixFilePermissions(dir, PosixFilePermissions.fromString("rwxrwxrwx"))

  var mounts = Vector.empty[String]
  if (Files.isDirectory(Path.of(url))) {
    mounts ++= Vector("-v", s"${Path.of(url).toRealPath()}:/src:ro")
    url = "file:///src"
  }

  val container = s"bonsai-community-${ProcessHandle.current().pid()}-${Random.nextInt(32768)}"
  val volume    = if (cache.nonEmpty) cache else container
  Runtime.getRuntime.addShutdownHook(new Thread(() => {
    quietly(engine, "rm", "-f", container)
    if (cache.isEmpty) quietly(engine, "volume", "rm", "-f", volume)
  }))

  val swap   = if (memorySwap.nonEmpty) memorySwap else memory
  val limits = Vector("--memory", memory, "--memory-swap", swap, "--pids-limit", "8192") ++
    (if (cpus.nonEmpty) Vector("--cpus", cpus) else Vector.empty)

  val command = Vector("timeout", "-k", "60", (limit + 900).toString, engine, "run", "--name", container) ++
    Vector("--userns=keep-id:uid=1000,gid=1000") ++
    Vector("--security-opt", "label=disable", "--security-opt", "no-new-privileges", "--cap-drop=all") ++
    limits ++
    Vector(
      "-v",
      s"$volume:/home/bonsai",
      "-v",
      s"${Path.of(ivy).toRealPath()}/local:/home/bonsai/.ivy2/local:ro",
      "-v",
      s"$dir:/out",
      "-v",
      s"${here.resolve("build.scala")}:/opt/community/build.scala:ro"
    ) ++
    mounts ++
    Vector(
      "-e",
      s"PROJECT_URL=$url",
      "-e",
      s"PROJECT_REF=$ref",
      "-e",
      s"JAVA_MAJOR=$java",
      "-e",
      s"PROJECT_TAG=$tag",
      "-e",
      s"BONSAI_INJECT=$inject",
      "-e",
      "BONSAI_MODE=relaxed",
      "-e",
      s"TIMEOUT_SECONDS=$timeoutSeconds",
      "-e",
      s"BONSAI_VERSION=$version",
      "-e",
      s"SBT_OPTS=-Dsbt.color=false -Dsbt.supershell=false -Xmx$heap",
      image,
      "scala-cli",
      "run",
      "--server=false",
      "--workspace",
      "/tmp/community",
      "/opt/community/build.scala",
      "--"
    ) ++ commands

  val code = new ProcessBuilder(command.asJava).inheritIO().start().waitFor()

  val status = dir.resolve("status")
  if (!Files.exists(status) || Files.size(status) == 0) Files.writeString(status, s"runner-error:$code\n")
  println(s"$name: ${Files.readString(status, UTF_8).replaceAll("\n+$", "")}")
}
