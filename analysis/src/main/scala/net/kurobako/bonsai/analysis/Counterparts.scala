package net.kurobako.bonsai.analysis

/** Code with the same role in a different shape: methods that use the same rarely used symbols of the build. */
object Counterparts {

  /** A method of the build with the symbols of the build its body names, other than its own locals. */
  case class Body(method: String, node: String, line: Int, uses: Set[String])

  /** A match in `method` that turns the `names` of the cases of the family `from` into cases of `to` of the same names.
    */
  case class Mirror(method: String, node: String, line: Int, from: String, to: String, names: Vector[String])

  /** An object of the build with the names of the types it defines. */
  case class Family(owner: String, node: String, line: Int, types: Set[String])

  object Model {
    val empty: Model = Model(Vector.empty, Vector.empty, Vector.empty)
  }

  case class Model(bodies: Vector[Body], mirrors: Vector[Mirror], families: Vector[Family])

  private val MaximumUsers   = 4
  private val MinimumShared  = 4
  private val MinimumOverlap = 0.4
  private val MinimumTypes   = 3
  private val MinimumMirror  = 2
  private val Shown          = 6

  def findings(model: Model, nodes: Map[String, Node]): Vector[Finding] =
    implementations(model.bodies, nodes) ++ mirrors(model.mirrors, nodes) ++ families(model.families, nodes)

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

  private def owner(name: String): String = name.take(name.lastIndexOf('.'))

  // A symbol many methods name says nothing of the role of one that does, so only the rarely named ones count, and
  // the members of the types that hold the methods are what those share by being one type.
  private def implementations(bodies: Vector[Body], nodes: Map[String, Node]): Vector[Finding] = {
    val users = bodies.zipWithIndex.flatMap((body, index) => body.uses.map(_ -> index)).groupMap(_._1)(_._2)
      .filter((_, indices) => indices.size <= MaximumUsers)
    val rare = users.toVector.flatMap((symbol, indices) =>
      indices.filter(index => owner(symbol) != owner(bodies(index).method))
    ).groupMapReduce(identity)(_ => 1)(_ + _)
    val shared = users.toVector.flatMap { (symbol, indices) =>
      val sorted = indices.sorted
      sorted.indices.flatMap(at => sorted.drop(at + 1).map(second => (sorted(at), second) -> symbol))
    }.groupMap(_._1)(_._2)
    shared.toVector.sortBy(_._1).flatMap { case ((first, second), symbols) =>
      val (left, right) = (bodies(first), bodies(second))
      val holders       = Set(owner(left.method), owner(right.method))
      val foreign       = symbols.filterNot(symbol => holders(owner(symbol)))
      val related       = left.method == right.method || left.uses(right.method) || right.uses(left.method)
      val overlap       = foreign.size.toDouble /
        rare.getOrElse(first, Int.MaxValue).min(rare.getOrElse(second, Int.MaxValue))
      Option.when(!related && foreign.sizeIs >= MinimumShared && overlap >= MinimumOverlap) {
        val names = foreign.map(Finding.simple(_)).distinct.sorted
        Finding.at(
          nodes,
          left.node,
          Finding.Rule.ParallelImplementation,
          s"${Finding.short(left.method)} and ${Finding.short(right.method)} use the same ${foreign.size} rarely " +
            s"used symbols of the build (${Finding.listed(names, Shown)}); they may implement one role twice",
          Finding.Level.Info,
          line = left.line
        )
      }.flatten
    }
  }
}
