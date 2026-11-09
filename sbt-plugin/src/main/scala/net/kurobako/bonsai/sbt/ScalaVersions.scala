package net.kurobako.bonsai.sbt

/** The Scala releases the tools serve: the compiler plugin, by the minor line it is built for, and the analysis, by the
  * TASTy its compiler reads.
  */
private[sbt] object ScalaVersions {
  private val Release  = """3\.(\d+)\.(\d+)""".r
  private val Prefixed = """3\.(\d+)\.(\d+)(?:-.*)?""".r

  private def parse(version: String): Option[(Int, Int)] = version match {
    case Release(minor, patch) => Some((minor.toInt, patch.toInt))
    case _                     => None
  }

  /** The line of the compiler plugin that runs in the compiler of the release. Every patch of a line from the oldest
    * the plugin runs in takes the plugin built for the line, which shares its compiler internals.
    */
  def plugin(version: String): Option[String] = parse(version).flatMap { case (minor, patch) =>
    val line   = s"3.$minor"
    val oldest = BonsaiArtifacts.oldest.get(line).flatMap(parse(_)).fold(0) { case (_, first) => first }
    Some(line).filter(_ => BonsaiArtifacts.lines.contains(line) && patch >= oldest)
  }

  private val Scalafix = """(\d+)\.(\d+)\.(\d+).*""".r

  private def numbers(version: String): Option[(Int, Int)] = version match {
    case Prefixed(minor, patch) => Some((minor.toInt, patch.toInt))
    case _                      => None
  }

  /** Whether the compiler of the release, or of a pre-release of it, takes the `-Wunused:all` that RemoveUnused reads.
    */
  def removeUnused(version: String): Boolean = numbers(version).exists(release =>
    parse(BonsaiArtifacts.removeUnusedFrom).exists(from => Ordering[(Int, Int)].gteq(release, from))
  )

  /** Whether RemoveUnused of the Scalafix release accepts the Scala release. Before 0.14.8 it takes 3.10 for 3.1. */
  def removeUnusedRule(scala: String, scalafix: String): Boolean = removeUnused(scala) &&
    (scalafix match {
      case Scalafix(major, minor, patch) =>
        Ordering[(Int, Int, Int)].gteq((major.toInt, minor.toInt, patch.toInt), (0, 14, 8)) ||
        numbers(scala).exists { case (scalaMinor, _) => scalaMinor < 10 }
      case _ => true
    })

  /** Whether scalafix can load the presentation compiler of the release, which ExplicitResultTypes types with. */
  def explicitResultTypes(version: String): Boolean =
    numbers(version).exists { case (minor, _) => !BonsaiArtifacts.resultTypesExcluded.contains(s"3.$minor") }

  /** Whether the release is of Scala 2, whose classes carry no TASTy for the analysis to read. */
  def scala2(version: String): Boolean = version.startsWith("2.")

  /** The log line naming the projects left out of the analysis as (project, Scala version) pairs, if any. */
  def skipped(projects: Seq[(String, String)]): Option[String] =
    if (projects.isEmpty) None
    else
      Some(
        "note: bonsai analyses Scala 3 only and left out " +
          projects.sorted.map { case (project, version) => s"$project (Scala $version)" }.mkString(", ")
      )

  /** Whether the compiler of the analysis reads the TASTy of the release, which a compiler does for its own minor line
    * and the older ones.
    */
  def readable(version: String): Boolean = parse(version).exists { case (project, _) =>
    parse(BonsaiArtifacts.analysisScala).exists { case (newest, _) => project <= newest }
  }
}
