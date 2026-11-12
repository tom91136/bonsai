package net.kurobako.bonsai.analysis

import java.nio.file.Path

/** The classpaths the reads of a manifest's compiles resolve against. */
private[analysis] class Readings(manifest: Manifest) {
  private val built = manifest.parts.zipWithIndex.toMap

  /** The entries a read of the compiles `members` resolves against: the classpaths of the JVM modules' compiles, then
    * those of the platform twins' and of the modules without outputs, so that where the compiler finds a class in
    * several it takes the first. Where the classpaths hold different versions of an artifact, the first is kept.
    */
  def of(members: Set[Output.Part]): Vector[Path] = {
    def rank(part: Output.Part) =
      (!built.contains(part), manifest.platforms(part._1), built.getOrElse(part, 0), part._1, part._2.ordinal)
    Classpath.readable(members.toVector.sortBy(rank(_)).flatMap(manifest.classpath.getOrElse(_, Vector.empty)).distinct)
  }

  /** The entries the read of the classes of the platform twin `twin` resolves against. */
  def twin(twin: String, ahead: Vector[Path], behind: Vector[Path]): Vector[Path] =
    Classpath.readable(ahead ++ manifest.classpathOf(Seq(twin)) ++ behind)
}
