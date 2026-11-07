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
  def plugin(version: String): Option[String] = parse(version).flatMap { (minor, patch) =>
    val line   = s"3.$minor"
    val oldest = BonsaiArtifacts.oldest.get(line).flatMap(parse(_)).fold(0)((_, first) => first)
    Option.when(BonsaiArtifacts.lines.contains(line) && patch >= oldest)(line)
  }

  private def numbers(version: String): Option[(Int, Int)] = version match {
    case Prefixed(minor, patch) => Some((minor.toInt, patch.toInt))
    case _                      => None
  }

  /** Whether the compiler of the release, or of a pre-release of it, takes the `-Wunused:all` that RemoveUnused reads.
    */
  def removeUnused(version: String): Boolean = numbers(version).exists(release =>
    parse(BonsaiArtifacts.removeUnusedFrom).exists(from => Ordering[(Int, Int)].gteq(release, from))
  )

  /** Whether scalafix can load the presentation compiler of the release, which ExplicitResultTypes types with. */
  def explicitResultTypes(version: String): Boolean =
    numbers(version).exists((minor, _) => !BonsaiArtifacts.resultTypesExcluded.contains(s"3.$minor"))

  /** Whether the compiler of the analysis reads the TASTy of the release, which a compiler does for its own minor line
    * and the older ones.
    */
  def readable(version: String): Boolean = parse(version).exists((project, _) =>
    parse(BonsaiArtifacts.analysisScala).exists((newest, _) => project <= newest)
  )
}
