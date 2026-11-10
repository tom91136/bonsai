package net.kurobako.bonsai.collections

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.matching.Regex

/** The findings of the compiler plugin in the audit mode, kept beside the class files in a record of each source, so
  * the output directory holds them as it holds the classes: a build that caches or prunes the classes does the same to
  * them. A record holds the digest of the source it was made from, so a reader can tell a record that is out of date.
  */
object Recorded {

  /** Where the records are in an output directory. */
  val Directory = "META-INF/bonsai/findings"

  /** What the strict mode would do with a finding: fail on an error, and report a warning or information. */
  enum Level(val id: String) {
    case Error   extends Level("error")
    case Warning extends Level("warning")
    case Info    extends Level("info")
  }

  case class Entry(rule: String, level: Level, line: Int, message: String)

  /** The findings of the source named `source` as it was with the content `digest`. */
  case class Record(source: String, digest: String, entries: List[Entry])

  /** The digest of the content of a source. */
  def digest(content: Array[Byte]): String = hex(MessageDigest.getInstance("SHA-256").digest(content))

  /** The name of a source in a record: its path from `root` with slashes, or its own path when it is not under it. */
  def key(root: Path, source: Path): String = {
    val base = root.toAbsolutePath.normalize
    val path = source.toAbsolutePath.normalize
    (if (path.startsWith(base)) base.relativize(path) else path).toString.replace(File.separatorChar, '/')
  }

  def file(output: Path, source: String): Path =
    output.resolve(
      Directory
    ).resolve(hex(MessageDigest.getInstance("SHA-256").digest(source.getBytes(UTF_8))).take(32) + ".tsv")

  def render(record: Record): String =
    record.entries.map(entry =>
      s"${entry.rule}\t${entry.level.id}\t${entry.line}\t${escaped(entry.message)}"
    ).mkString(s"source\t${escaped(record.source)}\t${record.digest}\n", "\n", if (record.entries.isEmpty) "" else "\n")

  def parse(text: String): Option[Record] =
    text.linesIterator.toList match {
      case header :: rest =>
        header.split("\t", -1) match {
          case Array("source", source, digest) =>
            val entries = rest.map(_.split("\t", -1) match {
              case Array(rule, level, line, message) =>
                for {
                  parsed <- Level.values.find(_.id == level)
                  number <- line.toIntOption
                } yield Entry(rule, parsed, number, unescaped(message))
              case _ => None
            })
            Option.when(entries.forall(_.isDefined))(Record(unescaped(source), digest, entries.flatten))
          case _ => None
        }
      case Nil => None
    }

  /** Writes the record in place of the one the source has. The old file is replaced rather than written through, as a
    * cache may have restored it as a link into a store that must stay as it was.
    */
  def write(output: Path, record: Record): Unit = {
    val target = file(output, record.source)
    val staged = target.resolveSibling(target.getFileName.toString + ".part")
    Files.createDirectories(target.getParent)
    Files.writeString(staged, render(record), UTF_8)
    Files.move(staged, target, StandardCopyOption.REPLACE_EXISTING): Unit
  }

  def read(file: Path): Option[Record] =
    if (Files.isRegularFile(file)) parse(Files.readString(file, UTF_8)) else None

  /** Every record file of an output directory in name order, with the record it holds if it holds one. */
  def all(output: Path): Vector[(Path, Option[Record])] = {
    val directory = output.resolve(Directory)
    if (!Files.isDirectory(directory)) Vector.empty
    else
      Using.resource(Files.list(directory))(_.toList.asScala.toVector).filter(_.getFileName.toString.endsWith(".tsv"))
        .sorted.map(file => file -> read(file))
  }

  private def hex(bytes: Array[Byte]): String = bytes.map(byte => f"$byte%02x").mkString

  private val Codes = Map('\\' -> "\\\\", '\t' -> "\\t", '\n' -> "\\n", '\r' -> "\\r")

  private def escaped(text: String): String = text.flatMap(character => Codes.getOrElse(character, character.toString))

  private val Escape = """\\(.)""".r

  private def unescaped(text: String): String =
    Escape.replaceAllIn(
      text,
      found =>
        Regex.quoteReplacement(found.group(1) match {
          case "t"   => "\t"
          case "n"   => "\n"
          case "r"   => "\r"
          case other => other
        })
    )
}
