package net.kurobako.bonsai.analysis

import Algebra.*

/** Design smells that need every call site, implementation or signature of the build as evidence. */
object Smells {

  /** A method of the build; `overriding` marks signatures fixed by a supertype, `exported` those other builds call as
    * part of a library's public API, `stored` the indices of the parameters its body only passes to constructors, and
    * `inlined` those of its inline parameters, which each expansion specialises on, and `rest` the fields of the term
    * parameter lists after the first; `tested` those its first statement branches on, when the body is known; an
    * `inline` method folds the literals it is passed at each expansion; `shared` marks an abstract method with several
    * implementations, whose literal arguments pick a layout the instances share.
    */
  case class Method(
      name: String,
      key: String,
      node: String,
      owner: String,
      fields: Vector[Algebra.Field],
      overriding: Boolean,
      exported: Boolean,
      test: Boolean,
      stored: Set[Int] = Set.empty,
      inlined: Set[Int] = Set.empty,
      inline: Boolean = false,
      shared: Boolean = false,
      suppressed: Set[String] = Set.empty,
      rest: Vector[Algebra.Field] = Vector.empty,
      tested: Option[Set[Int]] = None,
      line: Int = 0
  ) {
    def short: String = Finding.short(name)
  }

  /** How often a method returning a value is called, how often its value is dropped, and how often the code inverts it.
    */
  case class Usage(
      method: Method,
      result: String,
      calls: Int,
      discarded: Int,
      testCalls: Int,
      negated: Int = 0
  )

  /** A trait or abstract class with the concrete classes, objects and lambdas that implement it; `functional` when only
    * lambdas do, as for a trait standing in for a function type that would box its primitives, and `specialised` when
    * an implementation fixes one of its type parameters to a primitive, whose code the generic one would box.
    */
  case class Abstraction(
      name: String,
      node: String,
      module: String,
      implementations: Vector[(String, String)],
      functional: Boolean = false,
      specialised: Boolean = false
  )

  /** The type arguments given for one type parameter at every call or construction. */
  case class Instantiation(
      owner: String,
      node: String,
      parameter: String,
      arguments: Map[String, Int],
      varied: Boolean,
      testUses: Int
  )

  /** Calls that pass a parameter its own default value explicitly. */
  case class Default(
      owner: String,
      node: String,
      parameter: String,
      value: String,
      sites: Vector[Algebra.Site],
      line: Int
  )

  /** A parameter of a method that no caller leaves out, so its default is never used. */
  case class Spare(
      owner: String,
      node: String,
      parameter: String,
      sites: Vector[Algebra.Site],
      suppressed: Set[String],
      line: Int = 0
  )

  case class Model(
      methods: Vector[Method],
      sites: Map[String, Vector[Algebra.Site]],
      usages: Vector[Usage],
      abstractions: Vector[Abstraction],
      instantiations: Vector[Instantiation],
      defaults: Vector[Default],
      moduleUses: Vector[Modules.Use] = Vector.empty,
      spares: Vector[Spare] = Vector.empty
  )

  private val MinimumCalls          = 2
  private val MinimumInverted       = 4
  private val MinimumClumpFields    = 3
  private val MaximumClumpFields    = 6
  private val MaximumClumpSignature = 10
  private val MinimumClumpMethods   = 3
  private val WideClumpFields       = 4
  private val WideClumpMethods      = 5
  private val Shown                 = 6
  private val MinimumNulls          = 3
  private val Negations             = Set("not", "non", "no", "without", "never", "missing")
  private val FixedShown            = 8

  /** `hot` holds the nodes of the classes the build marks hot, when it marks any. */
  def findings(
      model: Model,
      products: Vector[Algebra.Product],
      nodes: Map[String, Node],
      hot: Option[Set[String]] = None,
      edges: Map[String, Set[String]] = Map.empty
  ): Vector[Finding] = {
    lazy val files     = fileEdges(nodes, edges)
    lazy val referrers = edges.toVector.flatMap((from, targets) => targets.map(_ -> from)).groupMap(_._1)(_._2)
    val reviewed       = model.methods.filterNot(_.overriding)
    // The callers in this build do not settle the API of a library, which other builds call.
    val checked = reviewed.view.filterNot(_.exported).flatMap { method =>
      // Parameters of recursive methods carry state, so callers fixing their initial value is the usual idiom.
      model.sites.get(method.key).filter(sites => !sites.exists(_.own) && sites.size >= MinimumCalls).toVector.flatMap {
        sites => parameters(method, sites).map((rule, detail, parameter) => (method, sites, rule, detail, parameter))
      }
    }.toVector
    val arguments = checked.flatMap { (method, sites, rule, detail, _) =>
      Finding.at(nodes, method.node, rule, detail, onlyTests = Algebra.onlyTests(sites), line = method.line)
    }
    val constant = checked.collect {
      case (method, _, Finding.Rule.ConstantArgument, _, Some(parameter)) => (method.node, method.short, parameter)
    }.toSet
    val results = model.usages.view.filter(usage =>
      !usage.method.overriding && !usage.method.exported && usage.calls >= MinimumCalls &&
        usage.discarded == usage.calls
    ).flatMap { usage =>
      val detail = s"${usage.method.short}: its ${usage.result} result is discarded at all ${usage.calls} calls"
      Finding.at(
        nodes,
        usage.method.node,
        Finding.Rule.IgnoredResult,
        detail,
        onlyTests = usage.testCalls == usage.calls,
        line = usage.method.line
      )
    }.toVector
    val inverted = model.usages.view.filter(usage =>
      usage.result.equalsIgnoreCase("boolean") &&
        !usage.method.overriding && !negated(usage.method) &&
        usage.calls >= MinimumInverted && usage.negated == usage.calls && usage.discarded == 0
    ).flatMap { usage =>
      val detail = s"${usage.method.short}: its ${usage.result} result is inverted at all ${usage.calls} calls"
      Finding.at(
        nodes,
        usage.method.node,
        Finding.Rule.InvertedBoolean,
        detail,
        onlyTests = usage.testCalls == usage.calls,
        line = usage.method.line
      )
    }.toVector
    val unused = model.usages.view.filter(usage =>
      !usage.method.overriding && usage.calls >= 1 &&
        usage.discarded == usage.calls &&
        // The rule of the ignored result reads the calls of methods called more than once; this one reads the single
        // call, and the API of a library, which the calls in this build do not settle.
        (usage.calls < MinimumCalls || usage.method.exported) &&
        !Finding.Rule.UnusedReturnValue.suppressedBy(usage.method.suppressed)
    ).flatMap { usage =>
      val detail = s"${usage.method.short}: its ${usage.result} result is never used"
      Finding.at(
        nodes,
        usage.method.node,
        Finding.Rule.UnusedReturnValue,
        detail,
        onlyTests = usage.testCalls == usage.calls,
        line = usage.method.line
      )
    }.toVector
    val modules = model.moduleUses.flatMap { use =>
      Finding.at(
        nodes,
        use.caller,
        Finding.Rule.ModuleRequirement,
        s"The module '${use.callerModule}' does not have the module '${use.targetModule}' in requirements"
      ).map(finding => finding.copy(node = finding.node.copy(source = use.source, line = use.line)))
    }
    val implementations =
      model.abstractions.view.filter(abstraction =>
        abstraction.implementations.size == 1 && abstraction.implementations.head._2 == abstraction.module &&
          !abstraction.functional && !abstraction.specialised &&
          !closesCycle(abstraction, abstraction.implementations.head._1, nodes, referrers, files)
      ).flatMap { abstraction =>
        Finding.at(
          nodes,
          abstraction.node,
          Finding.Rule.SingleImplementation,
          s"${abstraction.name} has a single implementation, ${abstraction.implementations.head._1}"
        )
      }.toVector
    val fixed = model.instantiations.filter(instantiation =>
      !instantiation.varied && instantiation.arguments.size == 1 && instantiation.arguments.values.sum >= MinimumCalls
    ).groupBy(_.node).toVector.sortBy(_._1).flatMap((node, group) => fixedParameters(node, group, nodes))
    val redundant = model.defaults.filterNot(default =>
      constant((default.node, default.owner, default.parameter))
    ).flatMap { default =>
      val detail = s"${default.owner}.${default.parameter} is passed its default ${default.value} explicitly at " +
        s"${default.sites.size} calls: ${Finding.listed(default.sites.map(_.location), Shown)}"
      Finding.at(
        nodes,
        default.node,
        Finding.Rule.RedundantDefault,
        detail,
        onlyTests = Algebra.onlyTests(default.sites),
        line = default.line
      )
    }
    val spare = model.spares.flatMap { spare =>
      val detail = s"${spare.owner}.${spare.parameter} has a default that none of its ${spare.sites.size} calls " +
        "relies on; remove the default"
      Finding.at(
        nodes,
        spare.node,
        Finding.Rule.UnusedDefault,
        detail,
        onlyTests = Algebra.onlyTests(spare.sites),
        suppressed = spare.suppressed,
        line = spare.line
      )
    }
    arguments ++ results ++ unused ++ inverted ++ modules ++ implementations ++ fixed ++ redundant ++ spare ++
      clumps(reviewed.filterNot(_.test), products, hot.getOrElse(Set.empty), model.sites).flatMap((key, detail, line) =>
        Finding.at(nodes, key, Finding.Rule.DataClump, detail, line = line)
      )
  }

  // A method named as the opposite of what it tests has no positive form to suggest.
  private def negated(method: Method): Boolean = {
    val bare = Finding.simple(method.name.takeWhile(_ != '(')).replaceFirst("^(is|has|does|can|should)(?=[A-Z])", "")
    bare.nonEmpty && Negations(s"${bare.head.toLower}${bare.tail.takeWhile(!_.isUpper)}")
  }

  // Naming the implementation in a file that uses the abstraction adds a file edge back, which closes a cycle when the
  // implementation's file already reaches that file.
  private def closesCycle(
      abstraction: Abstraction,
      implementation: String,
      nodes: Map[String, Node],
      referrers: Map[String, Vector[String]],
      files: Map[String, Set[String]]
  ): Boolean =
    nodes.get(implementation).exists { node =>
      val users = referrers.getOrElse(abstraction.node, Vector.empty).view.filter(_ != implementation)
        .flatMap(nodes.get(_).map(_.source)).toVector
      val reached = Liveness.reachable(files, Set(node.source))
      users.exists(user => user != node.source && reached(user))
    }

  private def fileEdges(nodes: Map[String, Node], edges: Map[String, Set[String]]): Map[String, Set[String]] =
    edges.toVector.flatMap { (from, targets) =>
      val reached = targets.toVector.flatMap(id => nodes.get(id).map(_.source))
      nodes.get(from).map(_.source).toVector.flatMap(source =>
        reached.collect { case target if target != source => source -> target }
      )
    }.groupMapReduce(_._1)(pair => Set(pair._2))(_ ++ _)

  /** The type parameters of the definitions of one type that every use gives the same type, in one finding that names
    * them, since the definitions of a type often repeat one parameter.
    */
  private def fixedParameters(
      node: String,
      group: Vector[Instantiation],
      nodes: Map[String, Node]
  ): Option[Finding] = {
    val onlyTests = group.forall(instantiation => instantiation.testUses == instantiation.arguments.values.sum)
    group match {
      case Vector(instantiation) =>
        val (argument, uses) = instantiation.arguments.head
        val detail           =
          s"${instantiation.owner}: type parameter ${instantiation.parameter} is always $argument across $uses uses"
        Finding.at(nodes, node, Finding.Rule.FixedTypeParameter, detail, onlyTests = onlyTests)
      case _ =>
        val entries = group.sortBy(_.owner).map { instantiation =>
          val own = instantiation.owner.stripPrefix(node).stripPrefix(".")
          val tag = if (own.isEmpty) instantiation.parameter else s"$own[${instantiation.parameter}]"
          s"$tag is ${instantiation.arguments.keys.head}"
        }
        val detail = s"${group.size} type parameters of ${Finding.simple(node)} are always given one type each " +
          s"(${Finding.listed(entries, FixedShown)})"
        Finding.at(nodes, node, Finding.Rule.FixedTypeParameter, detail, onlyTests = onlyTests)
    }
  }

  /** Parameters every caller passes the same value for, flags every caller passes a literal for, flags that together
    * take three of their four combinations, and references most callers leave null.
    */
  private def parameters(
      method: Method,
      sites: Vector[Algebra.Site]
  ): Vector[(Finding.Rule, String, Option[String])] = {
    // Java calls are kept at every arity, so a varargs call with another count is not a call of these parameters.
    val calls = sites.filter(_.arguments.size == method.fields.size)
    if (calls.isEmpty) Vector.empty
    else {
      val columns = calls.map(_.arguments).transpose
      // A flag the method only stores is data of the value it builds, which no branch splits, and an inline method is
      // specialised on its literal at every expansion already, and a flag tested deeper in the body leaves one body
      // shared, which a method per value would duplicate.
      def splits(index: Int) = method.fields(index).kind == Kind.Flag && !method.stored(index) && !method.inline &&
        method.tested.forall(_.contains(index))
      val literal = method.fields.indices.filter(index =>
        splits(index) && columns(index).forall(value => value == Argument.True || value == Argument.False)
      ).toVector
      val encoded = encoding(literal, columns)
      val states  = encoded.map { (first, second) =>
        (
          Finding.Rule.FlagArgument,
          s"${method.short}: ${method.fields(first).name} and ${method.fields(second).name} take only 3 of their " +
            s"4 combinations across ${calls.size} calls; they encode three states, which an enum names",
          None
        )
      }
      val paired = encoded.flatMap((first, second) => Vector(first, second)).toSet
      states ++ method.fields.zipWithIndex.view.flatMap { (field, index) =>
        val values = columns(index)
        val absent = values.count(_ == Argument.Absent)
        if (
          (field.kind == Kind.Other || field.kind == Kind.Text) && !untyped(field.shape) && !method.shared &&
          absent >= MinimumNulls && absent < values.size
        )
          Some(
            (
              Finding.Rule.ConstantArgument,
              s"${method.short}.${field.name} is null at $absent of ${values.size} calls and given a value at the others",
              Some(field.name)
            )
          )
        else if (field.kind == Kind.Other) None
        // A value the model does not know is not a constant one, and the rule says nothing of it.
        else if (values.exists(value => value == Argument.Unknown || value == Argument.Present)) None
        // the constant of an abstract method with several implementations picks a layout they share
        else if (values.distinct.size == 1)
          Option.when(!method.shared) {
            val plural = if (calls.size == 1) "call" else "calls"
            (
              Finding.Rule.ConstantArgument,
              s"${method.short}.${field.name} is always ${values.head.shown} across ${calls.size} $plural",
              Some(field.name)
            )
          }
        else
          Option.when(splits(index) && !paired(index)) {
            val enabled = values.count(_ == Argument.True)
            val detail  = s"${method.short}.${field.name} is a literal at all ${calls.size} calls " +
              s"($enabled true, ${calls.size - enabled} false); each value selects a different method"
            (Finding.Rule.FlagArgument, detail, Some(field.name))
          }
      }.toVector
    }
  }

  // Flags that between them take three of their four combinations are three states, which an enum names; each flag
  // belongs to at most one pair.
  private def encoding(flags: Vector[Int], columns: Vector[Vector[Argument]]): Vector[(Int, Int)] =
    flags.combinations(2).foldLeft(Vector.empty[(Int, Int)]) {
      case (taken, Vector(first, second))
          if !taken.exists((a, b) => a == first || b == first || a == second || b == second) &&
            columns(first).zip(columns(second)).distinct.size == 3 =>
        taken :+ (first -> second)
      case (taken, _) => taken
    }

  private val untyped =
    Set(Polynomial.atom("scala.Any"), Polynomial.atom("scala.AnyRef"), Polynomial.atom("java.lang.Object"))

  // A method every call of which passes literals is a row of a table, whose literals the callers chose.
  private def literalCalls(method: Method, sites: Map[String, Vector[Algebra.Site]]): Boolean =
    sites.get(method.key).exists(calls =>
      calls.nonEmpty && calls.forall(call =>
        call.arguments.size == method.fields.size && call.arguments.forall {
          case Algebra.Argument.Word(
                Algebra.Wording.Text,
                _
              ) | Algebra.Argument.Number(_) | Algebra.Argument.True | Algebra.Argument.False =>
            true
          case _ => false
        }
      )
    )

  /** Parameter groups that recur together across methods of different types, which a parameter object would name. */
  private def clumps(
      methods: Vector[Method],
      products: Vector[Algebra.Product],
      hot: Set[String],
      sites: Map[String, Vector[Algebra.Site]]
  ): Vector[(String, String, Int)] = {
    // An inline method is specialised at every expansion, which a parameter object would undo, and in hot code an
    // object of primitives allocates on every call.
    val signatures = methods.view.collect {
      case method if !method.inline && !literalCalls(method, sites) =>
        method -> (method.fields ++ method.rest).zipWithIndex.collect {
          case (field, index)
              if !method.inlined(index) && !field.contextual && !(field.primitive && hot(method.node)) &&
                !untyped(field.shape) =>
            s"${field.name}: ${field.shape}"
        }.distinct.sorted
    }.filter((_, items) => items.size >= MinimumClumpFields).toVector
    val support = signatures.flatMap { (method, items) =>
      val sizes =
        if (items.size > MaximumClumpSignature) Vector(MinimumClumpFields) else MinimumClumpFields to MaximumClumpFields
      sizes.flatMap(items.combinations(_).map(_.toSet -> method))
    }.groupMap(_._1)(_._2)
    val numeric = methods.flatMap(method =>
      (method.fields ++ method.rest).collect {
        case field if field.primitive && field.kind == Algebra.Kind.Number =>
          s"${field.name}: ${field.shape}" -> field.shape.toString
      }
    ).toMap
    val frequent = support.view.collect {
      case (items, users)
          if users.size >= MinimumClumpMethods && users.map(_.owner).distinct.size > 1 &&
            (items.size >= WideClumpFields || users.size >= WideClumpMethods) &&
            !(items.forall(numeric.contains) && items.map(numeric).size == 1) =>
        items -> users.toSet
    }.toMap
    val maximal = frequent.groupMap(_._2)(_._1).toVector.flatMap { (methods, candidates) =>
      candidates.collect { case items if Itemsets.maximal(items, candidates) => items -> methods }
    }.sortBy((items, methods) => (-items.size * methods.size, items.toVector.sorted.mkString))
    // Overlapping groups over mostly the same methods describe one parameter object.
    val chosen = maximal.foldLeft(Vector.empty[(Set[String], Set[Method])]) { (kept, candidate) =>
      val distinct = kept.forall((_, methods) =>
        methods.intersect(candidate._2).size * 2 < methods.union(candidate._2).size
      )
      if (distinct) kept :+ candidate else kept
    }
    lazy val shapes =
      products.sortBy(product => (product.fields.size, product.name)).map(product =>
        product -> product.fields.map(field => s"${field.name}: ${field.shape}").toSet
      )
    chosen.map { (items, methods) =>
      val owners   = methods.map(_.owner).size
      val modelled = shapes.collectFirst { case (product, fields) if items.subsetOf(fields) => product }
        .fold("")(product => s"; already the fields of ${Finding.simple(product.name)}")
      val first = methods.minBy(method => (method.name, method.node))
      (
        first.node,
        s"(${items.toVector.sorted.mkString(", ")}) recur in ${methods.size} methods across $owners types: " +
          Finding.listed(methods.toVector.map(_.short).sorted, Shown) + modelled,
        first.line
      )
    }
  }
}
