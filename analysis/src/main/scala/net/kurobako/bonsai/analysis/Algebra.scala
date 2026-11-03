package net.kurobako.bonsai.analysis

/** Checks over the algebraic shapes of products and sums, and over the values passed where products are built. */
object Algebra {

  /** A field or parameter; `primitive` when its type is a primitive, or an opaque type standing for one, and
    * `contextual` for a `using` or `implicit` parameter.
    */
  case class Field(name: String, shape: Polynomial, kind: Kind, primitive: Boolean = false, contextual: Boolean = false)

  /** Field kinds whose values can be read off construction sites and invariants. */
  enum Kind {
    case Optional, Flag, Number, Text, Other
    case Tag(cases: Vector[String])
  }

  enum Junction {
    case And, Or
  }

  /** How a predicate reads a field: an optional for presence, a flag for truth. */
  enum Probe {
    case Present, Flag
  }

  /** A `require` predicate over field indices; parts outside this vocabulary are `Unknown`. */
  enum Predicate {
    case Known(value: Boolean)
    case Holds(field: Int, probe: Probe)
    case Is(field: Int, value: String)
    case Compare(field: Int, operator: String, value: BigDecimal)
    case SameSize(left: Int, right: Int)
    case Not(predicate: Predicate)
    case Connective(junction: Junction, left: Predicate, right: Predicate)
    case Unknown

    def and(that: Predicate): Predicate = Connective(Junction.And, this, that)
    def or(that: Predicate): Predicate  = Connective(Junction.Or, this, that)
  }

  /** How construction is controlled: freely, only through the companion, or openly beside a validating factory. */
  enum Access {
    case Open, Private
    case Factory(names: Vector[String])
  }

  /** A sum alternative: a case class with its fields, or a singleton. */
  case class Variant(name: String, fields: Vector[Field]) {
    lazy val shape: Polynomial = Polynomial.product(fields.map(_.shape))
  }

  object Origin {
    def of(exported: Boolean): Origin = if (exported) Exported else Written
  }

  /** Why a product has the shape it has: `Written` by the author for this build alone, `Exported` as the API of a
    * library that other builds construct, or `Derived` when generic derivation builds and reads it field by field.
    * Derived wins where both hold, as twins of a derived shape are exempt from the duplicate rule.
    */
  enum Origin {
    case Written, Exported, Derived
  }

  /** A case class, or a method whose parameters stand in for fields when it has a precondition. */
  case class Product(
      name: String,
      node: String,
      fields: Vector[Field],
      parents: Set[String],
      invariant: Predicate,
      access: Access,
      origin: Origin,
      returns: Set[String] = Set.empty
  ) {
    def validated: Boolean = access != Access.Open || invariant != Predicate.Known(true)

    lazy val shape: Polynomial = Polynomial.product(fields.map(_.shape))
  }

  /** A sealed hierarchy of at least two case classes and objects without bodies, which an enum states directly;
    * `nested` when the cases are already members of the parent's companion, where an enum would keep them.
    */
  case class Enumeration(name: String, node: String, cases: Int, nested: Boolean, exported: Boolean)

  case class Sum(name: String, node: String, variants: Vector[Variant], accessors: Set[String] = Set.empty) {
    lazy val cases: Vector[Variant] = variants.filter(_.fields.nonEmpty)
  }

  /** Whether a spelled argument names an enum case or symbol, or is a string literal. */
  enum Wording {
    case Tag, Text
  }

  /** What a construction site is known to pass for a field. */
  enum Argument {
    case Present, Absent, True, False, Unknown
    case Number(value: BigDecimal)
    case Word(wording: Wording, value: String)

    def shown: String = this match {
      case Word(Wording.Tag, value)  => value
      case Number(value)             => value.toString
      case Word(Wording.Text, value) => s"\"$value\""
      case other                     => other.toString
    }
  }

  /** The arguments of one construction aligned with the product's fields, and where it happens. */
  case class Site(arguments: Vector[Argument], test: Boolean, own: Boolean, location: String)

  /** Whether every site is test code, so a conclusion drawn from them says more about tests than the design. */
  def onlyTests(sites: Vector[Site]): Boolean = sites.nonEmpty && sites.forall(_.test)

  /** Products and sums of the build; preconditions treat a method's parameters like the fields of a product, keyed by
    * the method key that `sites` uses.
    */
  case class Model(
      products: Vector[Product],
      sums: Vector[Sum],
      sites: Map[String, Vector[Site]],
      preconditions: Map[String, Product],
      enumerations: Vector[Enumeration]
  )

  /** Frequent itemsets, as shared field groups among sum cases or parameter groups among methods. */
  object Itemsets {

    /** Whether no larger candidate contains `items`. */
    def maximal(items: Set[String], candidates: Iterable[Set[String]]): Boolean =
      !candidates.exists(other => other.size > items.size && items.subsetOf(other))
  }

  /** Named fields, rendered as `name: shape`, shared by a set of sum alternatives. */
  private case class Factor(fields: Set[String], cases: Set[String])

  private val MinimumDuplicateWeight        = 2
  private val MinimumUnnamedDuplicateWeight = 5
  private val MinimumFactorSupport          = 3
  private val MinimumFactorCases            = 8
  private val MaxFactorFields               = 12
  private val MaxReportedFactors            = 3
  private val ShownNames                    = 8

  def findings(model: Model, nodes: Map[String, Node], intended: Intended = Intended()): Vector[Finding] = {
    val sums = model.sums.flatMap { sum =>
      val groups   = identical(sum)
      val repeated = groups.map(group =>
        s"${sum.name}: ${group.size} cases of shape ${group.head.shape}: ${names(group.map(_.name))}"
      )
      val factors =
        if (sum.cases.size < MinimumFactorCases) Vector.empty
        else commonFactors(sum, groups).take(MaxReportedFactors)
      repeated.flatMap(Finding.at(nodes, sum.node, Finding.Rule.IdenticalCases, _, Finding.Level.Info)) ++
        factors.flatMap { factor =>
          // Fields every alternative carries hoist out cleanly; a partial factor needs a new type and judgement.
          val level  = if (factor.cases.size == sum.variants.size) Finding.Level.Warning else Finding.Level.Info
          val shared = factor.fields.toVector.sorted.mkString(", ")
          val detail = s"${sum.name}: ${factor.cases.size} of ${sum.cases.size} cases share ($shared): " +
            names(factor.cases.toVector.sorted)
          Finding.at(nodes, sum.node, Finding.Rule.CommonFactor, detail, level)
        }
    }
    // Cases of sums are told apart by their names, as separate instructions or errors are, and identical-cases reports
    // those within one sum.
    def topPackage(name: String): Option[String] = {
      val first = name.takeWhile(_ != '.')
      if (first.length > 1 && first.forall(c => c.isLower || c == '_')) Some(first) else None
    }

    def moduleOf(product: Product): (Option[String], Option[String]) =
      (nodes.get(product.node).map(_.module), topPackage(product.name))

    def compatibleGroups(group: Vector[Product]): Vector[Vector[Product]] = {
      val isAllPublic = group.forall(_.access != Access.Private)
      val isOneModule = {
        val modules = group.map(moduleOf(_)._1).distinct
        val pkgs    = group.flatMap(p => topPackage(p.name)).distinct
        modules.size <= 1 && pkgs.size <= 1
      }
      if (isAllPublic || isOneModule) Vector(group)
      else {
        val publics = Option.when(group.count(_.access != Access.Private) > 1)(group.filter(_.access != Access.Private))
        val byModule = group.groupBy(moduleOf(_)).values.filter(_.size > 1).toVector
        (publics.toVector ++ byModule).distinct
      }
    }

    val byNode = model.products.map(product => product.node -> product).toMap
    // A builder returns the type it is nested in, so the two share their shape by design.
    def builder(product: Product): Boolean =
      nodes.get(product.node).flatMap(_.outer).flatMap(byNode.get).exists(outer =>
        product.returns(Finding.simple(outer.name)) && product.shape.key == outer.shape.key
      )

    val duplicates =
      model.products.sortBy(_.name).filter(product =>
        product.parents.isEmpty && product.shape.weight >= MinimumDuplicateWeight && !builder(product)
      )
        .groupBy(_.shape.key).values.view
        .flatMap { group =>
          val named = group.groupBy(_.fields.map(_.name).sorted).values.view.flatMap(compatibleGroups(_)).collect {
            case named if named.size > 1 => true -> named
          }.toVector
          val namedSizes = named.map(_._2.size).toSet
          // A wide shape is worth reporting whatever its fields are called, unless one set of names covers it already.
          val unnamed = compatibleGroups(group).view.collect {
            case g if g.head.shape.weight >= MinimumUnnamedDuplicateWeight && !namedSizes(g.size) =>
              false -> g
          }.toVector
          named ++ unnamed
        }
        .filter((_, group) =>
          group.map(_.node).distinct.size > 1 && !group.forall(_.origin == Origin.Derived) && !sameParent(group) &&
            !intended.settles(group.map(_.name))
        )
        .toVector.sortBy((named, group) => (!named, -group.head.shape.weight)).flatMap { (named, group) =>
          val kind      = if (named) "with the same field names" else "with different field names"
          val validated = group.filter(_.validated)
          val unchecked = group.filterNot(_.validated)
          // An unvalidated twin in tests represents the invalid inhabitants a validator must reject.
          val fixtures = unchecked.forall(product => nodes.get(product.node).exists(_.test))
          val labels   = group.map(_.name).zip(relative(group.map(_.name))).toMap
          val twin     =
            if (validated.isEmpty || unchecked.isEmpty) ""
            else s"; ${Finding.listed(unchecked.map(product => labels(product.name)), ShownNames)} skip the " +
              s"validation of ${Finding.listed(validated.map(product => labels(product.name)), ShownNames)}"
          val level  = if (named && !(validated.nonEmpty && fixtures)) Finding.Level.Warning else Finding.Level.Info
          val detail =
            s"${group.size} types $kind of shape ${group.head.shape}: ${names(group.map(_.name).sorted)}$twin"
          Finding.at(nodes, group.head.node, Finding.Rule.DuplicateShape, detail, level)
        }
    // Other builds construct a library's public products, so the constructions here do not settle their shape.
    val constructions = model.products.view.filter(_.origin == Origin.Written).flatMap { product =>
      model.sites.get(product.name).toVector.flatMap { sites =>
        construction(product, sites).flatMap((rule, detail) =>
          Finding.at(nodes, product.node, rule, detail, onlyTests = onlyTests(sites))
        )
      }
    }.toVector
    val enumerations = model.enumerations.flatMap { hierarchy =>
      val name                 = Finding.simple(hierarchy.name)
      val (level, consequence) =
        if (hierarchy.exported) Finding.Level.Info -> "as an enum it changes for the builds that use it"
        else if (hierarchy.nested) Finding.Level.Warning -> "an enum is equivalent"
        else Finding.Level.Info                          -> s"as an enum they move into $name"
      Finding.at(
        nodes,
        hierarchy.node,
        Finding.Rule.EnumCandidate,
        s"$name: ${hierarchy.cases} cases without bodies; $consequence",
        level
      )
    }
    sums ++ duplicates ++ constructions ++ enumerations
  }

  /** Names relative to their longest common prefix, so same-named types in different owners stay distinguishable. */
  private def relative(values: Vector[String]): Vector[String] = {
    val segments = values.map(_.split('.').toVector)
    val common   =
      segments.reduceOption((left, right) => left.view.zip(right).takeWhile(_ == _).map(_._1).toVector).fold(0)(_.size)
    segments.map(parts => parts.drop(common.min(parts.size - 1)).mkString("."))
  }

  private def names(values: Vector[String]): String = Finding.listed(relative(values), ShownNames)

  private def sameParent(group: Iterable[Product]): Boolean = group.map(_.parents).reduce(_ intersect _).nonEmpty

  /** Alternatives with the same non-trivial shape: `n*X` written out as `n` cases instead of one case over `n`. */
  private def identical(sum: Sum): Vector[Vector[Variant]] =
    sum.cases.groupBy(_.shape.key).values.filter(_.size > 1).toVector.sortBy(group => (-group.size, group.head.name))

  /** Named fields shared by several alternatives, which distributivity would hoist out: `X*A + X*B = X*(A + B)`. */
  private def commonFactors(sum: Sum, identical: Vector[Vector[Variant]]): Vector[Factor] = {
    val items = sum.cases.map(variant =>
      variant.name -> variant.fields.collect {
        case field if !sum.accessors(field.name) => s"${field.name}: ${field.shape}"
      }.distinct.sorted
    )
    val support = items.flatMap { (name, fields) =>
      val sizes = if (fields.size > MaxFactorFields) Vector(2) else 2 to 4
      sizes.flatMap(fields.combinations(_).map(_.toSet -> name))
    }.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    val common     = items.map(_._2.toSet).reduceOption(_ intersect _).getOrElse(Set.empty)
    val everywhere = Option.when(common.nonEmpty && sum.cases.size > 1)(common -> sum.cases.map(_.name).toSet).toMap
    val frequent   = support.filter((_, cases) => cases.size >= MinimumFactorSupport) ++ everywhere
    val groups     = identical.map(_.map(_.name).toSet)
    // A factor is maximal when no larger factor is shared by exactly the same alternatives.
    val maximal = frequent.groupMap(_._2)(_._1).view.flatMap { (cases, candidates) =>
      candidates.collect { case fields if Itemsets.maximal(fields, candidates) => Factor(fields, cases) }
    }.filterNot(factor => groups.exists(factor.cases.subsetOf)).toVector
      .sortBy(factor => (-factor.fields.size * factor.cases.size, factor.fields.toVector.sorted.mkString))
    // Greedily keep factors over mostly different alternatives; overlapping ones describe the same refactoring. Factors
    // over every alternative come first, whatever outranks them, since hoisting those is never a judgement call.
    val complete = maximal.filter(_.cases.size == sum.cases.size)
    complete ++ maximal.foldLeft(Vector.empty[Factor]) { (chosen, candidate) =>
      val distinct = (chosen ++ complete).forall(factor =>
        factor.cases.intersect(candidate.cases).size * 2 < factor.cases.union(candidate.cases).size
      )
      if (distinct && chosen.size < MaxReportedFactors) chosen :+ candidate else chosen
    }
  }

  /** Correlations between the optional and flag arguments of every construction of a product. */
  private def construction(product: Product, sites: Vector[Site]): Vector[(Finding.Rule, String)] = {
    val fields =
      product.fields.zipWithIndex.filter((field, _) => field.kind == Kind.Optional || field.kind == Kind.Flag)
    val optional                       = fields.filter(_._1.kind == Kind.Optional)
    val flags                          = fields.filter(_._1.kind == Kind.Flag)
    val name                           = Finding.simple(product.name)
    def complete(indices: Vector[Int]) = sites.filter(site => indices.forall(site.arguments(_) != Argument.Unknown))
    def evidence(determined: Int)      =
      s"$determined of ${sites.size} constructions determined" +
        (if (determined < sites.size) s", ${sites.size - determined} unknown" else "")
    val observed = sites.flatMap(_.arguments.zipWithIndex).groupMap(_._2)(_._1)
    val constant = fields.flatMap { (field, index) =>
      val values = observed.getOrElse(index, Vector.empty).distinct
      Option.when(sites.size > 1 && values.size == 1 && values.head != Argument.Unknown) {
        Finding.Rule.ConstantField ->
          s"$name.${field.name} is always ${values.head.shown} across ${sites.size} constructions"
      }
    }
    val exclusive = Option.when(optional.size > 1) {
      val indices    = optional.map(_._2)
      val determined = complete(indices)
      val present    = determined.map(site => indices.count(site.arguments(_) == Argument.Present))
      val varied     = indices.count(index => determined.exists(_.arguments(index) == Argument.Present))
      // A construction whose options are unknown may set both, so it leaves the correlation unsettled.
      Option.when(determined.size == sites.size && determined.size > 1 && varied > 1 && present.forall(_ <= 1)) {
        val exactly = if (present.forall(_ == 1)) "exactly" else "at most"
        val shapes  = optional.map(_._1.shape)
        val asSum   = Polynomial.One + Polynomial.sum(shapes.map(_ - Polynomial.One))
        val unused  = Polynomial.product(shapes) - asSum
        Finding.Rule.ExclusiveOptions ->
          (s"$name: $exactly one of (${optional.map(_._1.name).mkString(", ")}) is present; " +
            s"${evidence(determined.size)}; ${shapes.map(shape => s"($shape)").mkString("*")} used as $asSum, never $unused")
      }
    }.flatten
    // Like exclusive options, a flag is settled only when every construction is known, so the columns compare whole.
    def column(index: Int, value: Argument) =
      Option.when(sites.forall(_.arguments(index) != Argument.Unknown))(sites.map(_.arguments(index) == value))
    val presences = optional.flatMap((option, index) => column(index, Argument.Present).map(_ -> (option, index)))
      .groupMap(_._1)(_._2)
    val redundant = flags.flatMap { (flag, flagIndex) =>
      val matching = column(flagIndex, Argument.True).filter(_.distinct.size == 2).toVector.flatMap(truth =>
        presences.getOrElse(truth, Vector.empty) ++ presences.getOrElse(truth.map(!_), Vector.empty)
      )
      matching.sortBy(_._2).map((option, _) =>
        Finding.Rule.RedundantFlag ->
          s"$name.${flag.name} is determined by $name.${option.name}; ${evidence(sites.size)}"
      )
    }
    constant ++ exclusive ++ redundant
  }
}
