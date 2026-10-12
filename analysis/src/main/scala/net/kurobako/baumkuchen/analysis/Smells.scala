package net.kurobako.baumkuchen.analysis

import Algebra.*

/** Design smells that need every call site, implementation or signature of the build as evidence. */
object Smells {

  /** A method of the build; `overriding` marks signatures fixed by a supertype, and `exported` those other builds call
    * as part of a library's public API.
    */
  case class Method(
      name: String,
      node: String,
      owner: String,
      fields: Vector[Algebra.Field],
      overriding: Boolean,
      exported: Boolean,
      generated: Boolean,
      test: Boolean
  ) {
    def short: String = Finding.short(name)
  }

  /** How often a method returning a value is called, and how often its value is dropped. */
  case class Usage(method: Method, result: String, calls: Int, discarded: Int, testCalls: Int)

  /** A trait or abstract class with the concrete classes, objects and lambdas that implement it. */
  case class Abstraction(name: String, node: String, module: String, implementations: Vector[(String, String)])

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
  case class Default(owner: String, node: String, parameter: String, value: String, sites: Vector[Algebra.Site])

  case class Model(
      methods: Vector[Method],
      sites: Map[String, Vector[Algebra.Site]],
      usages: Vector[Usage],
      abstractions: Vector[Abstraction],
      instantiations: Vector[Instantiation],
      defaults: Vector[Default]
  )

  private val MinimumCalls          = 2
  private val MinimumClumpFields    = 3
  private val MaximumClumpFields    = 6
  private val MaximumClumpSignature = 10
  private val MinimumClumpMethods   = 3
  private val MaxReportedClumps     = 20
  private val Shown                 = 6

  def findings(model: Model, products: Vector[Algebra.Product], nodes: Map[String, Node]): Vector[Finding] = {
    val reviewed = model.methods.filterNot(method => method.overriding || method.generated)
    // The callers in this build do not settle the API of a library, which other builds call.
    val arguments = reviewed.filterNot(_.exported).flatMap { method =>
      // Parameters of recursive methods carry state, so callers fixing their initial value is the usual idiom.
      model.sites.get(method.name).filter(sites => !sites.exists(_.own) && sites.size >= MinimumCalls).toVector.flatMap {
        sites =>
          parameters(method, sites).flatMap(Finding.at(nodes, method.node, _, _, onlyTests = Algebra.onlyTests(sites)))
      }
    }
    val results = model.usages.filter(usage =>
      !usage.method.overriding && !usage.method.generated && !usage.method.exported && usage.calls >= MinimumCalls &&
        usage.discarded == usage.calls
    ).flatMap { usage =>
      val detail = s"${usage.method.short}: its ${usage.result} result is discarded at all ${usage.calls} calls"
      Finding.at(
        nodes,
        usage.method.node,
        Finding.Rule.IgnoredResult,
        detail,
        onlyTests = usage.testCalls == usage.calls
      )
    }
    val implementations = model.abstractions.filter(_.implementations.size == 1).flatMap { abstraction =>
      val (implementation, module) = abstraction.implementations.head
      // An implementation in another module is often a deliberate port; one beside its interface rarely is.
      val level = if (module == abstraction.module) Finding.Level.Warning else Finding.Level.Info
      Finding.at(
        nodes,
        abstraction.node,
        Finding.Rule.SingleImplementation,
        s"${abstraction.name} has a single implementation, $implementation",
        level
      )
    }
    val fixed = model.instantiations.filter(instantiation =>
      !instantiation.varied && instantiation.arguments.size == 1 && instantiation.arguments.values.sum >= MinimumCalls
    ).flatMap { instantiation =>
      val (argument, uses) = instantiation.arguments.head
      val detail           =
        s"${instantiation.owner}: type parameter ${instantiation.parameter} is always $argument across $uses uses"
      Finding.at(
        nodes,
        instantiation.node,
        Finding.Rule.FixedTypeParameter,
        detail,
        onlyTests = instantiation.testUses == uses
      )
    }
    val redundant = model.defaults.flatMap { default =>
      val detail = s"${default.owner}.${default.parameter} is passed its default ${default.value} explicitly at " +
        s"${default.sites.size} calls: ${Finding.listed(default.sites.map(_.location), Shown)}"
      Finding.at(
        nodes,
        default.node,
        Finding.Rule.RedundantDefault,
        detail,
        onlyTests = Algebra.onlyTests(default.sites)
      )
    }
    arguments ++ results ++ implementations ++ fixed ++ redundant ++
      clumps(reviewed.filterNot(_.test), products).flatMap((key, detail) =>
        Finding.at(nodes, key, Finding.Rule.DataClump, detail)
      )
  }

  /** Parameters every caller passes the same value for, and flags every caller passes a literal for. */
  private def parameters(method: Method, sites: Vector[Algebra.Site]): Vector[(Finding.Rule, String)] =
    method.fields.zipWithIndex.filter((field, _) => field.kind != Kind.Other).flatMap { (field, index) =>
      val values = sites.map(_.arguments(index))
      if (values.contains(Argument.Unknown)) None
      else if (values.distinct.size == 1)
        Some(Finding.Rule.ConstantArgument ->
          s"${method.short}.${field.name} is always ${values.head.shown} across ${sites.size} calls")
      else
        Option.when(field.kind == Kind.Flag) {
          val enabled = values.count(_ == Argument.True)
          val detail  = s"${method.short}.${field.name} is a literal at all ${sites.size} calls " +
            s"($enabled true, ${sites.size - enabled} false); each value selects a different method"
          Finding.Rule.FlagArgument -> detail
        }
    }

  /** Parameter groups that recur together across methods of different types, which a parameter object would name. */
  private def clumps(methods: Vector[Method], products: Vector[Algebra.Product]): Vector[(String, String)] = {
    val signatures = methods.filter(method => method.fields.size >= MinimumClumpFields).map(method =>
      method -> method.fields.map(field => s"${field.name}: ${field.shape}").distinct.sorted
    )
    val support = signatures.flatMap { (method, items) =>
      val sizes =
        if (items.size > MaximumClumpSignature) Vector(MinimumClumpFields) else MinimumClumpFields to MaximumClumpFields
      sizes.flatMap(items.combinations(_).map(_.toSet -> method))
    }.groupMap(_._1)(_._2)
    val frequent = support.filter((_, methods) =>
      methods.size >= MinimumClumpMethods && methods.map(_.owner).distinct.size > 1
    ).map((items, methods) => items -> methods.toSet)
    val maximal = frequent.groupMap(_._2)(_._1).toVector.flatMap { (methods, candidates) =>
      candidates.collect { case items if Itemsets.maximal(items, candidates) => items -> methods }
    }.sortBy((items, methods) => (-items.size * methods.size, items.toVector.sorted.mkString))
    // Overlapping groups over mostly the same methods describe one parameter object.
    val chosen = maximal.foldLeft(Vector.empty[(Set[String], Set[Method])]) { (kept, candidate) =>
      val distinct = kept.forall((_, methods) =>
        methods.intersect(candidate._2).size * 2 < methods.union(candidate._2).size
      )
      if (distinct && kept.size < MaxReportedClumps) kept :+ candidate else kept
    }
    chosen.map { (items, methods) =>
      val owners   = methods.map(_.owner).size
      val modelled = products.find(product =>
        items.subsetOf(product.fields.map(field => s"${field.name}: ${field.shape}").toSet)
      ).fold("")(product => s"; already the fields of ${product.name.split('.').last}")
      val first = methods.minBy(_.name)
      first.node ->
        (s"(${items.toVector.sorted.mkString(", ")}) recur in ${methods.size} methods across $owners types: " +
          Finding.listed(methods.toVector.map(_.short).sorted, Shown) + modelled)
    }
  }
}
