package net.kurobako.bonsai.analysis

import java.nio.file.PathMatcher
import net.kurobako.bonsai.java.{Model => JavaModel}
import net.kurobako.bonsai.lifting.*
import scala.collection.mutable
import scala.util.matching.Regex

/** Converts the model the javac plugin collected for the Java code of a build into the model of the analysis. */
private[analysis] object JavaConversion {

  /** The graph the javac plugin collected for the Java code of a build, with the methods, the fields and the calls it
    * found.
    */
  case class Collected(
      nodes: Map[String, Node],
      edges: Map[String, Set[String]],
      bodies: Map[Use, Set[String]],
      callers: Map[String, Set[Use]],
      roots: Set[Root],
      methods: Vector[Smells.Method],
      sites: Map[String, Vector[Algebra.Site]],
      guarded: Map[String, Vector[Algebra.Site]],
      usages: Vector[Smells.Usage],
      products: Vector[Algebra.Product],
      preconditions: Map[String, Algebra.Product],
      sums: Vector[Algebra.Sum],
      clones: Vector[Clones.Group],
      lints: Vector[JavaLints.Found],
      reflections: Vector[JavaModel.Reflection],
      costs: Vector[JavaModel.Cost],
      problems: Vector[String]
  )

  private case class Tally(total: Int = 0, dropped: Int = 0, tests: Int = 0, inverted: Int = 0)

  /** The graph of the Java code the javac plugin collected, which holds the nodes it found and the references between
    * them, and the methods, the fields and the calls it found.
    */
  def collected(
      manifest: Manifest,
      generated: Vector[PathMatcher],
      parallel: Parallel,
      reflective: Vector[Regex]
  ): Collected = {
    val nodes       = mutable.LinkedHashMap.empty[String, Node]
    val edges       = mutable.LinkedHashMap.empty[String, mutable.LinkedHashSet[String]]
    val bodies      = mutable.LinkedHashMap.empty[Use, mutable.LinkedHashSet[String]]
    val callers     = mutable.LinkedHashMap.empty[String, mutable.LinkedHashSet[Use]]
    val roots       = mutable.LinkedHashSet.empty[Root]
    val methods     = mutable.ArrayBuffer.empty[Smells.Method]
    val results     = mutable.LinkedHashMap.empty[String, String]
    val sites       = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[Algebra.Site]]
    val calls       = mutable.LinkedHashMap.empty[String, Tally]
    val products    = mutable.ArrayBuffer.empty[Algebra.Product]
    val required    = mutable.ArrayBuffer.empty[(String, Algebra.Product)]
    val sums        = mutable.ArrayBuffer.empty[Algebra.Sum]
    val fragments   = mutable.ArrayBuffer.empty[(JavaModel.Fragment, Boolean)]
    val lints       = mutable.ArrayBuffer.empty[JavaLints.Found]
    val reassigned  = mutable.ArrayBuffer.empty[JavaLints.Reassignable]
    val written     = mutable.Set.empty[(String, String)]
    val outside     = mutable.Set.empty[(String, String)]
    val reflections = mutable.ArrayBuffer.empty[JavaModel.Reflection]
    val costs       = mutable.LinkedHashMap.empty[String, JavaModel.Cost]
    val instances   = mutable.ArrayBuffer.empty[(JavaModel.Instance, Boolean)]
    val constructed = mutable.Set.empty[String]
    val members     = mutable.Map.empty[String, Vector[JavaModel.Field]]
    val kept        = mutable.Map.empty[(String, String), Map[String, JavaModel.Stored]]
    val problems    = Vector.newBuilder[String]
    val reads       = parallel.map(manifest.models)(model => JavaModel.read(model.path))
    manifest.models.zip(reads).foreach { (model, read) =>
      read match {
        case Left(problem)    => problems += problem
        case Right(collected) =>
          // The code of the test sources is marked as such, which lowers the findings that concern it.
          val test      = model.scope == Output.Scope.Test
          val published = manifest.libraries(model.module) && model.scope == Output.Scope.Main
          collected.nodes.foreach { node =>
            // The public API of a library is used by the builds that depend on it, as the public API of Scala code is.
            val exported = node.exported && published && node.outer.forall(nodes.get(_).exists(_.exported))
            nodes.getOrElseUpdate(
              node.name,
              Node(
                node.name,
                model.module,
                model.scope,
                node.source,
                node.line,
                node.generated || Outputs.isGenerated(generated, node.source),
                exported,
                node.outer,
                node.suppressed
              )
            ): Unit
            if (exported) roots += Root(node.name, Root.Kind.Exported)
            if (node.entry) roots += Root(node.name, Root.Kind.Main)
          }
          // The builds that depend on a library construct its public types, so the constructions in this build do not
          // settle the shape of one, as with a case class of Scala code.
          val exportedConstructible =
            if (published)
              collected.nodes.collect {
                case node if node.constructible && nodes.get(node.name).exists(_.exported) => node.name
              }.toSet
            else Set.empty[String]
          val returning = mutable.Map.empty[String, Set[String]]
          collected.methods.foreach { method =>
            val owner  = method.owner
            val result = returns(method)
            methods += Smells.Method(
              method.name,
              method.key,
              owner,
              owner,
              method.parameters.map(parameter => field(parameter)),
              // A method that overrides a method of a supertype is one it inherits rather than declares.
              overriding = method.overriding,
              // The public API of a library is used by the builds that depend on it, so the callers in this build do
              // not settle it.
              method.exported && manifest.libraries(model.module) && model.scope == Output.Scope.Main &&
                nodes.get(owner).forall(_.exported),
              test = test,
              method.stored,
              Set.empty,
              suppressed = method.suppressed
            )
            // The type the method returns, which the rules that need every call site read by.
            result.foreach { value =>
              results(method.key) = value
              returning(owner) = returning.getOrElse(owner, Set.empty) + value
            }
          }
          val constructions = collected.constructions.map(construction => construction.owner -> construction).toMap
          val recorded      = collected.products.map(_.name).toSet
          collected.fields.groupBy(_.owner).filterNot((owner, _) => recorded(owner)).foreach { (owner, fields) =>
            products += productOf(
              owner,
              owner,
              fields.map(found => field(found.parameter)),
              Set.empty,
              exported = exportedConstructible(owner),
              returns = returning.getOrElse(owner, Set.empty),
              construction = constructions.get(owner)
            )
          }
          // A record is a product, which the analysis reads alike with a case class.
          collected.products.foreach { product =>
            products += productOf(
              product.name,
              product.node,
              product.fields.map(field(_)),
              product.parents,
              exportedConstructible(product.node),
              returns = returning.getOrElse(product.node, Set.empty),
              construction = constructions.get(product.name)
            )
          }
          // A method that starts with guards is a product of its parameters, which the analysis reads alike with a
          // method of the Scala code that starts with a `require`.
          val declared = collected.methods.map(method => method.key -> method).toMap
          collected.preconditions.foreach { precondition =>
            declared.get(precondition.method).foreach { method =>
              val fields = method.parameters.map(field(_))
              required += method.key -> productOf(
                s"${method.name}(${method.parameters.map(_.name).mkString(", ")})",
                method.owner,
                fields,
                Set.empty,
                exported = false,
                invariant = predicate(precondition.guard, fields)
              )
            }
          }
          // A sealed type is a sum, which the analysis reads alike with a sealed trait.
          collected.sums.foreach { sum =>
            sums += Algebra.Sum(
              sum.name,
              sum.node,
              sum.variants.map(variant => Algebra.Variant(variant.name, Vector.empty))
            )
          }
          val owners = collected.methods.map(method => method.key -> method.owner).toMap
          collected.sites.foreach { site =>
            val callee = site.callee
            callers.getOrElseUpdate(callee, mutable.LinkedHashSet.empty) +=
              owners.get(site.caller).fold(Use(site.caller))(owner => Use(owner, Some(site.caller))): Unit
            sites.getOrElseUpdate(callee, mutable.ArrayBuffer.empty) += Algebra.Site(
              site.arguments.map(argument(_)),
              test = test,
              own = isOwn(site.caller, callee),
              site.location
            )
            // The calls of a method, those of them that drop the result, those in tests and those that invert it, which
            // the rules that need every call site read alike with the calls of the Scala code.
            val tally = calls.getOrElse(callee, Tally())
            calls(callee) = tally.copy(
              total = tally.total + 1,
              dropped = tally.dropped + (if (site.discarded) 1 else 0),
              tests = tally.tests + (if (test) 1 else 0),
              inverted = tally.inverted + (if (site.negated) 1 else 0)
            )
          }
          // A class is constructed by the code of any module, but only the module that declares it knows what its
          // constructors keep in its fields.
          instances ++= collected.instances.map(instance => instance -> test): Unit
          constructed ++= collected.constructions.map(_.owner): Unit
          members ++= collected.fields.groupBy(_.owner): Unit
          collected.stores.foreach { store =>
            val key = store.owner -> store.constructor
            kept(key) = kept.getOrElse(key, Map.empty) + (store.field -> store.kept)
          }
          collected.edges.foreach { edge =>
            if (edge.method.isEmpty) edges.getOrElseUpdate(edge.from, mutable.LinkedHashSet.empty) += edge.to: Unit
            else bodies.getOrElseUpdate(Use(edge.from, Some(edge.method)), mutable.LinkedHashSet.empty) += edge.to: Unit
          }
          fragments ++= collected.fragments.map(fragment => fragment -> test): Unit
          // The lints of the Java code are reported as findings of their own, with the module and the scope of the
          // model they were found in.
          collected.lints.foreach(lint =>
            lints += JavaLints.Found(lint, model.module, model.scope, Outputs.isGenerated(generated, lint.path))
          ): Unit
          // A field is written by any code of the build, so whether a field can be final is settled over all of it.
          collected.reassignables.foreach(field =>
            reassigned += JavaLints.Reassignable(
              field,
              model.module,
              model.scope,
              Outputs.isGenerated(generated, field.path)
            )
          ): Unit
          collected.writes.foreach { write =>
            written += write.owner                         -> write.name
            if (!write.constructor) outside += write.owner -> write.name
          }
          reflections ++= collected.reflections: Unit
          collected.costs.foreach(cost => costs.getOrElseUpdate(cost.key, cost)): Unit
      }
    }
    instances.foreach { (instance, test) =>
      if (constructed(instance.owner))
        members.get(instance.owner).foreach { fields =>
          val values = JavaModel.aligned(
            fields,
            kept.getOrElse(instance.owner -> instance.constructor, Map.empty),
            instance.arguments
          )
          sites.getOrElseUpdate(instance.owner, mutable.ArrayBuffer.empty) +=
            Algebra.Site(values.map(argument(_)), test, isOwn(instance.caller, instance.owner), instance.location)
        }
    }
    // A type the build names only in a string is reached from outside it, as the Scala code the build names is, and
    // the types inside it are reached with it.
    @annotation.tailrec
    def withInner(reached: Set[String]): Set[String] = {
      val grown = reached ++ nodes.values.filter(_.outer.exists(reached)).map(_.name)
      if (grown.size == reached.size) reached else withInner(grown)
    }
    withInner(nodes.keySet.filter(name => reflective.exists(Globs.matches(_, name))).toSet).foreach { name =>
      nodes.get(name).foreach(node => nodes.update(name, node.copy(exported = true)))
      roots += Root(name, Root.Kind.Reflective)
    }
    lints ++= JavaLints.unwritten(reassigned.toVector, written.toSet, outside.toSet, nodes.toMap)
    // The usages of the methods of the Java code, which the rules that need every call site read alike with the usages
    // of the methods of the Scala code.
    val usages = methods.toVector.flatMap { method =>
      for {
        returns <- results.get(method.key)
        tally   <- calls.get(method.key)
      } yield Smells.Usage(method, returns, tally.total, tally.dropped, tally.tests, tally.inverted)
    }
    val built = products.reverseIterator.map(product => product.name -> product).toMap
    // The fields of a variant are those of the product of it, which a record that extends the sealed type is and which
    // the model of another module may hold.
    val resolved = sums.toVector.map(sum =>
      sum.copy(variants =
        sum.variants.map(variant => variant.copy(fields = built.get(variant.name).fold(variant.fields)(_.fields)))
      )
    )
    Collected(
      nodes.toMap,
      edges.view.mapValues(_.toSet).toMap,
      bodies.view.mapValues(_.toSet).toMap,
      callers.view.mapValues(_.toSet).toMap,
      roots.toSet,
      methods.toVector,
      sites.view.filterKeys(!built.contains(_)).mapValues(_.toVector).toMap,
      sites.view.filterKeys(built.contains).map((callee, found) =>
        callee -> named(found.toVector, built(callee))
      ).toMap ++ required.flatMap { (callee, product) =>
        val calls = sites.getOrElse(callee, mutable.ArrayBuffer.empty).filter(_.arguments.size == product.fields.size)
        Option.when(calls.nonEmpty)(callee -> calls.toVector)
      },
      usages,
      products.toVector,
      required.toMap,
      resolved,
      // The copies of the code of the Java code are of the whole build, as the same block of code written in two
      // modules of it is as much a copy as one written twice in one.
      JavaFragments.groups(fragments),
      lints.toVector,
      reflections.toVector,
      costs.values.toVector,
      problems.result()
    )
  }

  /** Whether a call is one the callee makes of itself, whose arguments the method itself fixes, or a construction
    * inside the type it builds.
    */
  private def isOwn(caller: String, callee: String): Boolean = caller == callee || caller.startsWith(s"$callee.")

  /** The type a method of the Java code returns, unless it returns nothing or returns the type that holds it, whose
    * result the code that calls it is meant to drop.
    */
  private def returns(method: JavaModel.Method): Option[String] = {
    val returned = method.returns
    if (returned.isEmpty || returned == "void") None
    else if (returned == Finding.simple(method.owner)) None
    else Some(returned)
  }

  /** The argument a call passes, which the analysis reads the values the callers pass by. */
  private def argument(argument: String): Algebra.Argument =
    argument match {
      case "true"                               => Algebra.Argument.True
      case "false"                              => Algebra.Argument.False
      case "absent"                             => Algebra.Argument.Absent
      case "unknown"                            => Algebra.Argument.Unknown
      case value if value.startsWith("number:") => Algebra.Argument.Number(BigDecimal(value.drop("number:".length)))
      case value if value.startsWith("text:") => Algebra.Argument.Word(Algebra.Wording.Text, value.drop("text:".length))
      case value if value.startsWith("symbol:") =>
        Algebra.Argument.Word(Algebra.Wording.Tag, value.drop("symbol:".length))
      case _ => Algebra.Argument.Present
    }

  /** The product with the invariant its constructor starts with, and who may call the constructor. */
  private def productOf(
      name: String,
      node: String,
      fields: Vector[Algebra.Field],
      parents: Set[String],
      exported: Boolean,
      invariant: Algebra.Predicate = Algebra.Predicate.Known(true),
      returns: Set[String] = Set.empty,
      construction: Option[JavaModel.Construction] = None
  ): Algebra.Product =
    constructed(
      Algebra.Product(
        name,
        name,
        node,
        fields,
        parents,
        invariant,
        Algebra.Access.Open,
        Algebra.Origin.of(exported),
        returns = returns
      ),
      construction
    )

  private def constructed(product: Algebra.Product, construction: Option[JavaModel.Construction]): Algebra.Product =
    construction.fold(product) { construction =>
      product.copy(
        invariant = predicate(construction.guard, product.fields),
        access = construction.access match {
          case "private" => Algebra.Access.Private
          case "factory" => Algebra.Access.Factory(construction.factories)
          case _         => Algebra.Access.Open
        }
      )
    }

  /** The predicate a guard over the names of fields is, over their positions. */
  private def predicate(guard: JavaModel.Guard, fields: Vector[Algebra.Field]): Algebra.Predicate = {
    val indices                                            = fields.map(_.name).zipWithIndex.toMap
    def of(name: String)(accepts: Algebra.Kind => Boolean) =
      indices.get(name).filter(index => accepts(fields(index).kind))
    def read(guard: JavaModel.Guard): Algebra.Predicate = guard match {
      case JavaModel.Guard.Known(value)                         => Algebra.Predicate.Known(value)
      case JavaModel.Guard.Holds(name, JavaModel.Probe.Present) =>
        of(name)(_ == Algebra.Kind.Optional)
          .fold(Algebra.Predicate.Unknown)(Algebra.Predicate.Holds(_, Algebra.Probe.Present))
      case JavaModel.Guard.Holds(name, JavaModel.Probe.Flag) =>
        of(name)(_ == Algebra.Kind.Flag).fold(Algebra.Predicate.Unknown)(Algebra.Predicate.Holds(_, Algebra.Probe.Flag))
      case JavaModel.Guard.Is(name, value) =>
        of(name)(_.isInstanceOf[Algebra.Kind.Tag]).fold(Algebra.Predicate.Unknown)(Algebra.Predicate.Is(_, value))
      case JavaModel.Guard.Compare(name, operator, value) =>
        of(name)(_ == Algebra.Kind.Number).fold(Algebra.Predicate.Unknown)(Algebra.Predicate.Compare(
          _,
          operator,
          value
        ))
      case JavaModel.Guard.Not(inner)                                      => Algebra.Predicate.Not(read(inner))
      case JavaModel.Guard.Connective(JavaModel.Junction.And, left, right) => read(left).and(read(right))
      case JavaModel.Guard.Connective(JavaModel.Junction.Or, left, right)  => read(left).or(read(right))
      case JavaModel.Guard.Unknown                                         => Algebra.Predicate.Unknown
    }
    read(guard)
  }

  /** The constructions of a product with the enum cases they pass named by the case alone, as its invariant names them.
    */
  private def named(sites: Vector[Algebra.Site], product: Algebra.Product): Vector[Algebra.Site] =
    sites.map(site =>
      site.copy(arguments = site.arguments.zip(product.fields).map {
        case (Algebra.Argument.Word(Algebra.Wording.Tag, value), field) if field.kind.isInstanceOf[Algebra.Kind.Tag] =>
          Algebra.Argument.Word(Algebra.Wording.Tag, value.substring(value.lastIndexOf('.') + 1))
        case (argument, _) => argument
      })
    )

  /** The parameter of a method, or the field of a type, which the analysis compares the methods of a build by. */
  private def field(parameter: JavaModel.Parameter): Algebra.Field =
    Algebra.Field(
      parameter.name,
      shape(parameter.kind, parameter.shape),
      if (parameter.kind == "other" && parameter.shape == "java.lang.String") Algebra.Kind.Text
      else kind(parameter.kind, parameter.cases),
      parameter.primitive
    )

  /** The shape of a type, which the analysis compares the parameters of the methods by. */
  private def shape(kind: String, name: String): Polynomial = kind match {
    case "flag"     => Polynomial.constant(2)
    case "optional" => Polynomial.One + Polynomial.atom(name)
    case _          => Polynomial.atom(name)
  }

  /** The kind of a type, which the analysis reads the parameters of the methods by. */
  private def kind(kind: String, cases: Vector[String]): Algebra.Kind = kind match {
    case "flag"     => Algebra.Kind.Flag
    case "number"   => Algebra.Kind.Number
    case "optional" => Algebra.Kind.Optional
    case "tag"      => Algebra.Kind.Tag(cases)
    case _          => Algebra.Kind.Other
  }

  /** The graph of the build, which holds the nodes of the Scala code and of the Java code alike. */
  def merge(graph: Graph, collected: Collected): Graph =
    graph.copy(
      nodes = graph.nodes ++ collected.nodes,
      declared = Graph.union(graph.declared, collected.edges),
      roots = graph.roots ++ collected.roots,
      bodies = Graph.union(graph.bodies, collected.bodies)
    )

}
