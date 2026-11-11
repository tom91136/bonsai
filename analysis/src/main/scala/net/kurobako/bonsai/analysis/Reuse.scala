package net.kurobako.bonsai.analysis

import java.io.File
import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.security.MessageDigest
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.Using
import scala.util.matching.Regex

/** Keeps the report of a run beside the manifest, so that a run whose inputs are the same prints it again instead of
  * extracting the build once more.
  */
private[analysis] object Reuse {
  val Marker = "bonsai: inputs unchanged, report reused"

  case class Shown(failing: Boolean, text: String)

  /** What a run printed: the lines for the error stream, and the report lines, each with whether it fails. */
  case class Record(diagnostics: Vector[String], report: Vector[Shown]) {
    def problems: Vector[String] = diagnostics.filter(_.startsWith(Problem))
  }

  /** Starts the line of something the analysis could not do, and of something it did without. */
  val Problem = "problem: "
  val Note    = "note: "

  def store(manifest: Path): Path = manifest.resolveSibling("report.reuse")

  /** The scan of the texts in the form the key reads. */
  def rendered(texts: Either[String, Vector[Finding]], excluded: Vector[Finding]): Vector[String] =
    texts.fold(problem => Vector(s"$Problem$problem"), _.map(_.toString)) ++ excluded.map(_.toString)

  /** Names everything the report depends on: the arguments, the manifest and the files it names, the baseline and the
    * purity lock (`named`), the code that runs, and the texts `scanned` finds.
    */
  def key(
      args: List[String],
      manifestPath: Path,
      manifest: Manifest,
      named: Vector[Path],
      parallel: Parallel,
      scanned: () => Vector[String]
  ): String = {
    val digest                             = MessageDigest.getInstance("SHA-256")
    def addBytes(bytes: Array[Byte]): Unit = {
      digest.update(ByteBuffer.allocate(Integer.BYTES).putInt(bytes.length).array)
      digest.update(bytes)
    }
    def add(text: String): Unit = addBytes(text.getBytes(UTF_8))
    val own                     = sys.props("java.class.path").split(File.pathSeparator).toVector.collect {
      case entry if entry.nonEmpty => Path.of(entry)
    }
    args.foreach(add(_))
    addBytes(Files.readAllBytes(manifestPath))
    parallel.map(
      inputs(paths(manifest) ++ named ++ own)
    )((label, content) => hashed(label, content)).foreach(addBytes(_))
    scanned().foreach(add(_))
    digest.digest.map("%02x".format(_)).mkString
  }

  /** The paths the analysis reads, which are those of the outputs, sources, models and classpaths, and the release file
    * of the target JDK, whose classes are read in place.
    */
  def paths(manifest: Manifest): Vector[Path] =
    (manifest.outputs ++ manifest.sources ++ manifest.models ++ manifest.resources).map(_.path) ++
      manifest.classpath.toVector.sortBy(_._1).flatMap(_._2) ++ manifest.jdk.map(_.resolve("release"))

  // Jars are immutable where a dependency cache keeps them, and cost seconds to read in full.
  private def inputs(paths: Vector[Path]): Vector[(String, Option[Path])] =
    paths.map(_.toAbsolutePath.normalize).distinct.flatMap { path =>
      if (Files.isDirectory(path))
        (s"directory $path", None) +: Outputs.files(path).map(file => (s"file ${path.relativize(file)}", Some(file)))
      else if (Files.isRegularFile(path) && path.toString.endsWith(".jar"))
        Vector((s"jar $path ${Files.size(path)} ${Files.getLastModifiedTime(path).toMillis}", None))
      else if (Files.isRegularFile(path)) Vector((s"file $path", Some(path)))
      else Vector((s"missing $path", None))
    }

  private def hashed(label: String, content: Option[Path]): Array[Byte] = {
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(label.getBytes(UTF_8))
    content.foreach { file =>
      val buffer = Buffers.get()
      Using.resource(Files.newInputStream(file))(feed(_, digest, buffer))
    }
    digest.digest
  }

  @tailrec
  private def feed(in: InputStream, digest: MessageDigest, buffer: Array[Byte]): Unit = {
    val read = in.read(buffer)
    if (read >= 0) {
      digest.update(buffer, 0, read)
      feed(in, digest, buffer)
    }
  }

  private val Buffers = ThreadLocal.withInitial(() => Array.ofDim[Byte](1 << 16))

  /** The record `file` holds for `key`, unless it holds another key or cannot be read. */
  def load(file: Path, key: String): Option[Record] =
    Try(Files.readAllLines(file).asScala.toVector).toOption.flatMap {
      case head +: lines if head == key =>
        lines.foldLeft[Option[Record]](Some(Record(Vector.empty, Vector.empty))) { (record, line) =>
          record.flatMap(found =>
            line.split("\t", 2) match {
              case Array("err", text) =>
                unescaped(text).map(text => found.copy(diagnostics = found.diagnostics :+ text))
              case Array("out", rest) =>
                rest.split("\t", 2) match {
                  case Array(flag @ ("true" | "false"), text) =>
                    Some(found.copy(report = found.report :+ Shown(flag == "true", text)))
                  case _ => None
                }
              case _ => None
            }
          )
        }
      case _ => None
    }

  def save(file: Path, key: String, record: Record): Unit = {
    val lines = key +:
      (record.diagnostics.map(text => s"err\t${escaped(text)}") ++
        record.report.map(shown => s"out\t${shown.failing}\t${shown.text}"))
    val next = file.resolveSibling(s"${file.getFileName}.next")
    Files.writeString(next, lines.mkString("", "\n", "\n"))
    Files.move(next, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING): Unit
  }

  private def escaped(text: String): String = text.flatMap {
    case '\\'  => "\\\\"
    case '\n'  => "\\n"
    case '\r'  => "\\r"
    case other => other.toString
  }

  private val Escape = """\\(.?)""".r

  private def unescaped(text: String): Option[String] =
    Option.when(Escape.findAllMatchIn(text).forall(found =>
      found.group(1).nonEmpty && "nr\\".contains(found.group(1))
    )) {
      Escape.replaceAllIn(
        text,
        found =>
          Regex.quoteReplacement(found.group(1) match {
            case "n" => "\n"
            case "r" => "\r"
            case _   => "\\"
          })
      )
    }
}
