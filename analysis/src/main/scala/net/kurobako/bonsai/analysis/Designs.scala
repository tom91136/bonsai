package net.kurobako.bonsai.analysis

/** Operations kept apart from the type they act on: an object of functions over one type beside it, an alias with an
  * object of operations, functions that only call their first parameter, and one method per width where one generic
  * method would do.
  */
object Designs {

  /** A method of a type of the build. `module` holds when its owner is an object, and `instance` when that object
    * implements a trait, as a type-class instance does; `first` is the class of its first parameter, and `receiving`
    * whether its body calls nothing but that parameter and nothing of its owner, and that parameter is neither a
    * primitive nor a `String`.
    */
  case class Method(
      name: String,
      owner: String,
      node: String,
      module: Boolean,
      instance: Boolean,
      public: Boolean,
      first: Option[String],
      parameters: Vector[String],
      result: String,
      receiving: Boolean,
      test: Boolean,
      line: Int = 0
  ) {
    def short: String = Finding.simple(name)
  }

  /** A transparent type alias beside an object of the same name holding `operations` methods; `tupled` when it names a
    * collection of tuples.
    */
  case class Alias(name: String, node: String, target: String, tupled: Boolean, operations: Int)

  object Model {
    val empty: Model = Model(Vector.empty, Vector.empty)
  }

  case class Model(methods: Vector[Method], aliases: Vector[Alias]) {
    def excluding(named: String => Boolean): Model =
      Model(methods.filterNot(found => named(found.node)), aliases.filterNot(found => named(found.node)))

    def joined(other: Model): Model = Model(methods ++ other.methods, aliases ++ other.aliases)
  }

  // Fewer methods are a coincidence of naming rather than a design.
  private val MinimumFamily = 3
  private val Shown         = 4
  private val Widths        = "#+".r

  def findings(model: Model, nodes: Map[String, Node]): Vector[Finding] =
    detached(model.methods, nodes) ++ aliased(model.aliases, nodes) ++ receivers(model.methods, nodes) ++
      numbered(model.methods, nodes)

  /** Objects named for a type, as its plural, whose public methods all take that type first. */
  private def detached(methods: Vector[Method], nodes: Map[String, Node]): Vector[Finding] =
    methods.filter(method => method.module && method.public && !method.instance).groupBy(_.owner).toVector.sortBy(_._1)
      .flatMap { (owner, members) =>
        val firsts = members.map(_.first).distinct
        firsts match {
          case Vector(Some(tpe))
              if members.size > 1 && nodes.contains(tpe) && plural(Finding.simple(owner), Finding.simple(tpe)) =>
            Finding.at(
              nodes,
              members.head.node,
              Finding.Rule.DetachedCompanion,
              s"object ${Finding.simple(owner)} holds ${members.size} methods that take ${article(Finding.simple(tpe))} first " +
                s"(${Finding.listed(members.map(_.short).distinct.sorted, Shown)}); make them members or extension methods in " +
                s"the companion of ${Finding.simple(tpe)}",
              onlyTests = members.forall(_.test)
            )
          case _ => None
        }
      }

  private def plural(owner: String, tpe: String): Boolean =
    owner == s"${tpe}s" || owner == s"${tpe}es" || tpe.endsWith("y") && owner == s"${tpe.dropRight(1)}ies"

  private def aliased(aliases: Vector[Alias], nodes: Map[String, Node]): Vector[Finding] =
    aliases.view.filter(_.operations > 0).flatMap { alias =>
      val element = if (alias.tupled) ", with a case class for the elements its tuples stand for" else ""
      Finding.at(
        nodes,
        alias.node,
        Finding.Rule.AliasWithOperations,
        s"type ${Finding.simple(alias.name)} = ${alias.target} has ${alias.operations} " +
          s"${if (alias.operations == 1) "operation" else "operations"} in an object of its name; " +
          s"make it a class or an opaque type that holds them$element"
      )
    }.toVector

  /** Families of functions in one object that call nothing but the parameter they take first. */
  private def receivers(methods: Vector[Method], nodes: Map[String, Node]): Vector[Finding] =
    methods.filter(_.receiving).groupBy(method => (method.owner, method.first)).toVector.sortBy(_._1._1).flatMap {
      case ((owner, Some(tpe)), members) if members.size >= MinimumFamily =>
        val owned = if (nodes.contains(tpe)) s"members of ${Finding.simple(tpe)} or " else ""
        Finding.at(
          nodes,
          members.head.node,
          Finding.Rule.ReceiverParameter,
          s"${members.size} methods of ${Finding.simple(owner)} take ${article(Finding.simple(tpe))} first and only call it " +
            s"(${Finding.listed(members.map(_.short).distinct.sorted, Shown)}); make them ${owned}extension methods of it",
          onlyTests = members.forall(_.test),
          line = members.head.line
        )
      case _ => None
    }

  /** Sibling methods named and typed alike but for a width, one per width where one generic method would do. */
  private def numbered(methods: Vector[Method], nodes: Map[String, Node]): Vector[Finding] =
    methods.filter(method => Finding.Numbers.findAllIn(method.short).size == 1)
      .groupBy(method => (method.owner, Finding.numberless(method.short))).toVector.sortBy(
        _._1
      ).flatMap {
        (_, members) =>
          val shapes = members.map(method => (method.parameters :+ method.result).map(unsized(_)))
          // Methods taking the same parameters cannot be overloads of one another, and those alike in body take the
          // width as a parameter, as near-duplicate-code says.
          val same = members.map(_.parameters).distinct.size == 1
          Option.when(members.map(_.short).distinct.size >= MinimumFamily && shapes.distinct.size == 1 && !same)(
            members
          )
            .flatMap(family =>
              Finding.at(
                nodes,
                family.head.node,
                Finding.Rule.NumberedFamily,
                s"${Finding.listed(family.map(_.short).distinct.sorted, Shown)} differ only in a width and share " +
                  "one signature; overload them on their typed parameters, take a type parameter or a type class, " +
                  "or define each in its type's companion",
                onlyTests = family.forall(_.test)
              )
            )
      }

  /** A type with the widths in its names made alike, and its type arguments, such as `SInt[8]`, kept as they are. */
  private def unsized(tpe: String): String = {
    val marked = tpe.foldLeft((0, Vector.empty[Char])) { case ((depth, kept), character) =>
      val nested = depth + (if (character == '[') 1 else if (character == ']') -1 else 0)
      (nested, kept :+ (if (depth == 0 && character.isDigit) '#' else character))
    }._2.mkString
    Widths.replaceAllIn(marked, "#")
  }

  private def article(word: String): String = if ("AEIOUaeiou".contains(word.head)) s"an $word" else s"a $word"
}
