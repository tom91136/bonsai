package net.kurobako.bonsai.analysis

import java.nio.file.Path
import scala.annotation.tailrec

/** The classpaths the reads of a manifest's compiles resolve against. */
private[analysis] class Readings(manifest: Manifest) {
  private val built    = manifest.parts.zipWithIndex.toMap
  private val requires = manifest.requires
  private val inside   = (manifest.parts ++ manifest.classpath.keySet).distinct.groupBy(_._1)

  private def direct(part: Output.Part): Vector[Output.Part] =
    Option.when(part._2 == Output.Scope.Test)(part._1 -> Output.Scope.Main).filter(inside(part._1).contains).toVector ++
      requires.getOrElse(part._1, Set.empty).toVector.sorted.flatMap(inside.getOrElse(_, Vector.empty))
        .filter(other => part._2 == Output.Scope.Test || other._2 == Output.Scope.Main)

  /** The compiles that `members` were compiled against, apart from themselves, the nearest first: the main scope of the
    * module of a test scope, and the compiles of the modules they depend on, of the same or the main scope.
    */
  def required(members: Set[Output.Part]): Vector[Output.Part] = {
    @tailrec def from(frontier: Vector[Output.Part], seen: Vector[Output.Part]): Vector[Output.Part] = {
      val known = seen.toSet ++ members
      val next  = frontier.flatMap(direct).distinct.filterNot(known)
      if (next.isEmpty) seen else from(next, seen ++ next)
    }
    from(members.toVector.sortBy(rank(_)), Vector.empty)
  }

  private def rank(part: Output.Part) =
    (!built.contains(part), manifest.platforms(part._1), built.getOrElse(part, 0), part._1, part._2.ordinal)

  /** The entries a read of the compiles `members` resolves against: the classpaths of the JVM modules' compiles, then
    * those of the platform twins' and of the modules without outputs, so that where the compiler finds a class in
    * several it takes the first, and behind them those of the compiles they require, whose classes the read reaches.
    * Where the classpaths hold different versions of an artifact, the first is kept.
    */
  def of(members: Set[Output.Part]): Vector[Path] = Classpath.readable(entries(members))

  /** The entries of the classpaths a read of `members` takes, before versions are chosen. */
  def entries(members: Set[Output.Part]): Vector[Path] =
    (members.toVector.sortBy(rank(_)) ++ required(members)).flatMap(manifest.classpath.getOrElse(_, Vector.empty))
      .distinct

  /** The entries the read of the classes of the platform twin `twin` resolves against. */
  def twin(twin: String, ahead: Vector[Path], behind: Vector[Path]): Vector[Path] =
    Classpath.readable(ahead ++ entries(inside.getOrElse(twin, Vector.empty).toSet) ++ behind)
}
