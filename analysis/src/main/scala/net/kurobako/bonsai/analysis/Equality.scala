package net.kurobako.bonsai.analysis

import scala.collection.mutable

/** Case classes with fields that compare by reference or change after construction, which equality and hashing then
  * misread. Such a class is reported only when a value of it is compared somewhere in the build, directly or inside
  * another value that is.
  */
object Equality {

  /** A field of a case class that makes its equality unstable, and why. */
  case class Field(name: String, reason: String)

  /** A case class of the build with its unstable `fields`, the build classes it extends and the ones its fields hold,
    * by qualified name.
    */
  case class Case(
      name: String,
      node: String,
      fields: Vector[Field],
      parents: Set[String],
      holds: Set[String],
      suppressed: Set[String]
  )

  /** A place that compares values of the build classes `types`, in the way `how` says, at `site`. */
  case class Comparison(types: Set[String], how: String, site: String, test: Boolean)

  object Model {
    val empty: Model = Model(Vector.empty, Vector.empty)
  }

  case class Model(cases: Vector[Case], comparisons: Vector[Comparison]) {
    def joined(other: Model): Model = Model(cases ++ other.cases, comparisons ++ other.comparisons)
  }

  private case class Reach(comparison: Comparison, holder: Option[String])

  def findings(model: Model, nodes: Map[String, Node]): Vector[Finding] = {
    val reached = reaching(model)
    model.cases.filter(found => found.fields.nonEmpty && nodes.get(found.node).exists(!_.generated)).flatMap { found =>
      reached.get(found.name).flatMap { reach =>
        val simple = Finding.simple(found.name)
        val how    = reach.holder match {
          case Some(holder) =>
            s"$simple is held by ${Finding.simple(holder)}, which is compared ${reach.comparison.how}"
          case None => s"a value of $simple is compared ${reach.comparison.how}"
        }
        Finding.at(
          nodes,
          found.node,
          Finding.Rule.UnstableEquality,
          s"${found.fields.map(field => s"${field.name}: ${field.reason}").mkString("; ")}, so $simple has " +
            s"unstable equality, and $how at ${reach.comparison.site}",
          onlyTests = reach.comparison.test,
          suppressed = found.suppressed
        )
      }
    }
  }

  /** The classes whose values are compared, each with the comparison that reaches it and the class holding it when that
    * comparison is of the holder. Comparisons in main code are followed before those in tests.
    */
  private def reaching(model: Model): Map[String, Reach] = {
    val byName                       = model.cases.groupBy(_.name)
    val children                     = model.cases.flatMap(found => found.parents.map(_ -> found)).groupMap(_._1)(_._2)
    def covering(types: Set[String]) =
      types.toVector.flatMap(name => byName.getOrElse(name, Vector.empty) ++ children.getOrElse(name, Vector.empty))
    val reached = mutable.LinkedHashMap.empty[String, Reach]
    val queue   = mutable.Queue.empty[Case]
    def reach(found: Case, comparison: Comparison, holder: Option[Case]): Unit =
      if (!reached.contains(found.name)) {
        reached(found.name) = Reach(comparison, holder.map(_.name))
        queue += found
      }
    val comparisons = model.comparisons.groupBy(_.test)
    Vector(false, true).foreach { test =>
      comparisons.getOrElse(test, Vector.empty).sortBy(comparison => (comparison.site, comparison.how)).foreach(
        comparison => covering(comparison.types).foreach(reach(_, comparison, None))
      )
      while (queue.nonEmpty) {
        val holder = queue.dequeue()
        covering(holder.holds).foreach(reach(_, reached(holder.name).comparison, Some(holder)))
      }
    }
    reached.toMap
  }
}
