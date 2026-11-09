package net.kurobako.bonsai.analysis

/** Code with the same role in a different shape: matches that translate one family of cases into a same-named one, and
  * objects that define the same types.
  */
object Counterparts {

  /** A match in `method` that turns the `names` of the cases of the family `from` into cases of `to` of the same names.
    */
  case class Mirror(method: String, node: String, line: Int, from: String, to: String, names: Vector[String])

  /** An object of the build with the names of the types it defines. */
  case class Family(owner: String, node: String, line: Int, types: Set[String])

  object Model {
    val empty: Model = Model(Vector.empty, Vector.empty)
  }

  case class Model(mirrors: Vector[Mirror], families: Vector[Family]) {
    def joined(other: Model): Model = Model(mirrors ++ other.mirrors, families ++ other.families)
  }

  private val MinimumTypes  = 3
  private val MinimumMirror = 2
  private val Shown         = 6

  def findings(model: Model, nodes: Map[String, Node]): Vector[Finding] =
    mirrors(model.mirrors, nodes) ++ families(model.families, nodes)

  private def mirrors(mirrors: Vector[Mirror], nodes: Map[String, Node]): Vector[Finding] =
    mirrors.filter(_.names.sizeIs >= MinimumMirror).flatMap { mirror =>
      Finding.at(
        nodes,
        mirror.node,
        Finding.Rule.MirroredCases,
        s"${Finding.short(mirror.method)} turns ${mirror.names.mkString(", ")} of ${Finding.simple(mirror.from)} into " +
          s"the cases of ${Finding.simple(mirror.to)} of the same names; the two may be one type",
        Finding.Level.Info,
        line = mirror.line
      )
    }

  // Objects that define the same types around different data are one design written twice.
  private def families(families: Vector[Family], nodes: Map[String, Node]): Vector[Finding] = {
    val named = families.flatMap(family => family.types.map(_ -> family)).groupMap(_._1)(_._2)
    val pairs = named.values.toVector.flatMap { holders =>
      val sorted = holders.sortBy(_.owner)
      sorted.indices.flatMap(at => sorted.drop(at + 1).map(sorted(at) -> _))
    }.distinct.sortBy((first, second) => (first.owner, second.owner)).filter((first, second) =>
      first.types.intersect(second.types).sizeIs >= MinimumTypes
    )
    pairs.flatMap { (first, second) =>
      val shared = first.types.intersect(second.types)
      Option.when(
        shared.sizeIs >= MinimumTypes && shared.size * 2 >= first.types.size.min(second.types.size)
      )(
        Finding.at(
          nodes,
          first.node,
          Finding.Rule.ParallelFamily,
          s"${Finding.simple(first.owner)} and ${Finding.simple(second.owner)} both define " +
            s"${Finding.listed(shared.toVector.sorted, Shown)}; they may be one design written twice",
          Finding.Level.Info,
          line = first.line
        )
      ).flatten
    }
  }
}
