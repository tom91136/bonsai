package net.kurobako.bonsai.analysis

import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

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
      val next  = frontier.flatMap(direct(_)).distinct.filterNot(known)
      if (next.isEmpty) seen else from(next, seen ++ next)
    }
    from(members.toVector.sortBy(rank(_)), Vector.empty)
  }

  private def rank(part: Output.Part) =
    (!built.contains(part), manifest.platforms(part._1), built.getOrElse(part, 0), part._1, part._2.ordinal)

  private val takes = TrieMap.empty[Output.Part, Map[String, (Vector[BigInt], Path)]]

  /** The version of each artifact the compile `part` takes, with its jar. */
  def taken(part: Output.Part): Map[String, (Vector[BigInt], Path)] =
    takes.getOrElseUpdate(part, Classpath.taken(manifest.classpath.getOrElse(part, Vector.empty)))

  private def chosen(members: Set[Output.Part], behind: Iterable[Map[String, (Vector[BigInt], Path)]]) =
    Classpath.newest(required(members).map(taken(_)) ++ behind) ++ Classpath.newest(members.toVector.map(taken(_)))

  /** The entries a read of the compiles `members` resolves against: the classpaths of the JVM modules' compiles, then
    * those of the platform twins' and of the modules without outputs, so that where the compiler finds a class in
    * several it takes the first, and behind them those of the compiles they require, whose classes the read reaches. Of
    * each artifact the read takes the newest version a member takes, or that a compile they require takes where no
    * member holds it.
    */
  def of(members: Set[Output.Part]): Vector[Path] = Classpath.keeping(entries(members), chosen(members, Vector.empty))

  /** The entries of the classpaths a read of `members` takes, before versions are chosen: those of the compiles, merged
    * so that each keeps the order of its own classpath, then those of the compiles they require, likewise. A class a
    * build shadows thus resolves to the copy the compile that names it read, whichever compile came first.
    */
  def entries(members: Set[Output.Part]): Vector[Path] =
    merged(members.toVector.sortBy(rank(_)).map(part => manifest.classpath.getOrElse(part, Vector.empty))) ++
      merged(required(members).map(part => manifest.classpath.getOrElse(part, Vector.empty)))

  /** The entries of `classpaths` in an order that keeps each classpath's own. Entries no classpath orders between come
    * in the order they first appear; where classpaths disagree, their first appearance decides.
    */
  private def merged(classpaths: Vector[Vector[Path]]): Vector[Path] = {
    val listed = classpaths.flatten.distinct
    if (classpaths.lengthCompare(1) <= 0) listed
    else {
      val position = listed.zipWithIndex.toMap
      val ordering = Ordering.by(position)
      val edges    = classpaths.flatMap { entries =>
        val kept = entries.distinct
        kept.zip(kept.drop(1))
      }.distinct
      val following = edges.groupMap(_._1)(_._2)
      val waiting   = mutable.Map.from(edges.groupMapReduce(_._2)(_ => 1)(_ + _))
      val pending   = mutable.TreeSet.from(listed)(using ordering)
      val order     = Vector.newBuilder[Path]
      while (pending.nonEmpty) {
        val next = pending.find(path => waiting.getOrElse(path, 0) == 0).getOrElse(pending.head)
        pending -= next
        order += next
        following.getOrElse(next, Vector.empty).foreach(after =>
          waiting.update(after, waiting.getOrElse(after, 0) - 1)
        )
      }
      order.result()
    }
  }

  /** The entries the read of the classes of the platform twin `twin` resolves against: its outputs and those of its
    * peers `ahead`, its own classpaths, and those of its run, `behind`, the versions of its own first.
    */
  def twin(twin: String, ahead: Vector[Path], behind: Vector[Path]): Vector[Path] = {
    val members = inside.getOrElse(twin, Vector.empty).toSet
    Classpath.keeping(ahead ++ entries(members) ++ behind, chosen(members, Vector(Classpath.taken(behind))))
  }
}
