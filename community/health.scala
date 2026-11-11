// What one project left in its output directory, and whether bonsai or the project's own build is behind a bad result.

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*

case class Failure(kind: String, evidence: Vector[String])

case class Output(
    name: String,
    status: String,
    log: Vector[String],
    err: Vector[String],
    crash: Vector[String],
    runs: Vector[(String, Int)],
    failing: Set[String],
    findings: Vector[String]
)

object Output {
  private val ansi = "\u001B\\[[0-9;]*[A-Za-z]".r

  private def lines(file: Path): Vector[String] =
    if (Files.isRegularFile(file)) Files.readAllLines(file, ISO_8859_1).asScala.toVector.map(ansi.replaceAllIn(_, ""))
    else Vector.empty

  private def listed(dir: Path, suffix: String): Vector[Path] =
    if (Files.isDirectory(dir)) {
      val stream = Files.list(dir)
      try stream.iterator.asScala.filter(_.getFileName.toString.endsWith(suffix)).toVector.sortBy(_.toString)
      finally stream.close()
    } else Vector.empty

  def read(dir: Path): Output = {
    val info = dir.resolve("info")
    val runs = lines(info.resolve("runs.tsv")).flatMap(_.split('\t') match {
      case Array(id, exit, _*) => exit.toIntOption.map(id -> _)
      case _                   => None
    })
    val outputs = listed(info, "manifest.tsv.out").map(file => file -> lines(file))
    Output(
      name = dir.getFileName.toString,
      status = lines(dir.resolve("status")).headOption.map(_.takeWhile(_ != ':').trim).filter(_.nonEmpty)
        .getOrElse("missing"),
      log = lines(dir.resolve("build.log")),
      err = listed(info, ".err").flatMap(lines(_)),
      crash = lines(dir.resolve("crash.txt")),
      runs = runs,
      failing = outputs.collect {
        case (file, out) if out.exists(_.startsWith("error\t")) => file.getFileName.toString.stripSuffix(".out")
      }.toSet,
      findings = outputs.flatMap(_._2).sorted
    )
  }
}

object Health {
  val Shown = 6

  private val banner  = "bonsai: \\w+ mode, \\d+ of \\d+ analysis rules active".r
  private val phrases = Vector("could not be read", "could not be analysed", "cannot be unpickled")
  private val stops   = Set("timeout", "oom", "killed")
  private val frame   = "^\\s+at net\\.kurobako\\.bonsai\\.".r
  private val absent  = Vector(
    "^note: undefined: .+ # -?\\d+: .+ at readTasty$".r,
    "^note: .+ could not be \\w+: dotty\\.tools\\.dotc\\.core\\.MissingType: .*$".r
  )
  private val rerun = "^(\\[\\w+\\]\\s+)?read in \\d+ groups: ".r

  def missingClass(line: String): Boolean = absent.exists(_.matches(line))

  def clash(line: String): Boolean =
    rerun.findPrefixOf(line).isEmpty &&
      (line.contains("is a class in") && line.contains("a package in") ||
        line.contains("is a class and a package on the classpath"))

  def missing(output: Output): Vector[String] = (output.log ++ output.err).distinct.filter(missingClass)

  def truncated(line: String): String = {
    val ascii = line.map(c => if (c < ' ' && c != '\t' || c > '~') '?' else c)
    if (ascii.length > 300) ascii.take(297) + "..." else ascii
  }

  private def limited(hits: Vector[String]): Vector[String] =
    hits.take(Shown) ++ Option.when(hits.size > Shown)(s"... and ${hits.size - Shown} more")

  def analysed(output: Output)
      : Boolean = (output.log.iterator ++ output.err.iterator).exists(banner.findFirstIn(_).isDefined)

  private def thrown(log: Vector[String]): Vector[String] = {
    val first = log.indexWhere(frame.findFirstIn(_).isDefined)
    if (first < 0) Vector.empty
    else {
      val header = log.take(first).reverseIterator.find(l => !l.startsWith(" ") && !l.startsWith("\t")).toVector
      header ++ log.drop(first).takeWhile(frame.findFirstIn(_).isDefined).take(4)
    }
  }

  def assess(output: Output): Vector[Failure] = {
    val all                                                             = (output.log ++ output.err).distinct
    val ran                                                             = analysed(output)
    def hit(kind: String, matching: String => Boolean): Option[Failure] =
      Option(all.filter(matching)).filter(_.nonEmpty).map(hits => Failure(kind, limited(hits)))

    val uncaught = output.log.dropWhile(banner.findFirstIn(_).isEmpty).filter(_.startsWith("Exception in thread"))
    val exits    = output.runs.collect {
      case (id, code) if code != 0 && !output.failing(id) => s"the analysis of $id exited with $code"
    }
    val judged   = output.runs.exists(_._2 != 0) && exits.isEmpty
    val jvm      = if (ran) output.crash.filter(l => l.startsWith("== ") && l.contains("hs_err_pid")) else Vector.empty
    val crash    = thrown(output.log) ++ (if (uncaught.isEmpty) Vector.empty else limited(uncaught)) ++ exits ++ jvm

    Vector(
      hit("analysis-failed", _.contains("bonsai analysis failed")).filterNot(_ => judged),
      hit(
        "problem",
        l =>
          l.startsWith("problem: ") || !missingClass(l) && (l.startsWith("note: ") || phrases.exists(l.contains))
      ),
      hit("clash", clash),
      Option.when(crash.nonEmpty)(Failure("crash", crash)),
      Option.when(ran && stops.contains(output.status))(
        Failure("stopped", s"status ${output.status} while the analysis was running" +: output.log.takeRight(3))
      )
    ).flatten
  }
}
