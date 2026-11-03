package net.kurobako.bonsai.analysis

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.ConstantType
import dotty.tools.dotc.core.Types.ExprType
import dotty.tools.dotc.core.Types.TermRef
import dotty.tools.dotc.core.Types.ThisType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.util.Spans.Span
import net.kurobako.bonsai.effects.Axioms
import net.kurobako.bonsai.effects.Effect
import net.kurobako.bonsai.effects.Kind
import net.kurobako.bonsai.effects.Receiver
import net.kurobako.bonsai.lifting.Calls
import scala.collection.mutable
import scala.runtime.ScalaRunTime
import scala.util.control.NonFatal

/** Side effects of the build's methods, inferred from their bodies and from axioms for the standard library. A call
  * takes its callee's effects, and the effects of the functions it passes that the callee runs; mutating a value the
  * method itself allocated is invisible to its callers. What neither the build nor the axioms describe is unknown,
  * which is never taken for pure.
  */
object Purity {

  /** A statement whose value is dropped although computing it has no effect. */
  case class Discard(callee: String, node: String, location: String, suppressed: Set[String])

  /** A function passed to a transformation of a collection, option or result, which performs `effect`; a `view` runs it
    * on every traversal.
    */
  case class Transform(
      operation: String,
      effect: String,
      node: String,
      location: String,
      view: Boolean,
      suppressed: Set[String]
  )

  /** A kind of effect a method has, shown by its first cause, whether it acts on the method's receiver alone, and
    * whether a library body it reaches performs it.
    */
  case class Observed(kind: Kind, shown: String, receiver: Boolean, library: Boolean)

  /** An authored method: the node it belongs to, what is known of its effects, each kind of effect it has, and the
    * contract it fulfils for collections, sorting and pattern matching, such as `equals`, if any.
    */
  case class Method(
      node: String,
      verdict: Verdict,
      effects: Vector[Observed],
      role: Option[String],
      suppressed: Set[String]
  )

  /** An assignment to `variable`, which other threads share, in a function `entry` runs on another thread. */
  case class SharedWrite(variable: String, entry: String, node: String, location: String, suppressed: Set[String])

  object Policy {

    /** The kinds a policy names, by id. */
    val Kinds: Map[String, Kind] =
      Vector(Kind.Reads, Kind.Writes, Kind.Input, Kind.Output, Kind.Random, Kind.Unknown).map(kind =>
        kind.toString.toLowerCase(java.util.Locale.ROOT) -> kind
      ).toMap

    /** A policy written `prefix=kind,kind`, with nothing after `=` for none. */
    def parse(text: String): Either[String, Policy] = text.split("=", 2) match {
      case Array(prefix, kinds) if prefix.nonEmpty =>
        val ids     = kinds.split(',').view.map(_.trim).filter(_.nonEmpty).toSet
        val unknown = ids -- Kinds.keySet
        Either.cond(
          unknown.isEmpty,
          Policy(prefix, ids.map(Kinds)),
          s"unknown effect kinds ${unknown.toSeq.sorted.mkString(", ")} in $text; the kinds are " +
            Kinds.keys.toSeq.sorted.mkString(", ")
        )
      case _ => Left(s"an effects policy is written prefix=kind,kind, not $text")
    }
  }

  /** The kinds of effects the methods of the packages and classes `prefix` names may have; `*` in the prefix stands for
    * any one name, and throwing is always allowed.
    */
  case class Policy(prefix: String, allowed: Set[Kind]) {
    private val segments = prefix.split('.').toVector

    /** How specific the policy is for `name`, a qualified method name, when it covers it. */
    def covers(name: String): Option[(Int, Int)] = {
      val path = name.takeWhile(_ != '(').split('.').toVector
      Option.when(
        segments.size <= path.size && segments.zip(path).forall((segment, part) => segment == "*" || segment == part)
      )((segments.size, -segments.count(_ == "*")))
    }
  }

  /** Whether an effect is proven, or may only hide in a call without known effects, which `Unknown` names. */
  enum Certainty {
    case Known
    case Unknown(origin: String)
  }

  /** What is known of a method's effects, with the first effect that decides it. */
  enum Verdict {
    case Pure
    case Effectful(cause: String, certainty: Certainty)
  }

  object Model {
    val empty: Model = Model(Vector.empty, Vector.empty, Vector.empty, Map.empty, library = 0)
  }

  /** `methods` holds the authored methods of the build by qualified name; `library` counts the library bodies read. */
  case class Model(
      discards: Vector[Discard],
      transforms: Vector[Transform],
      writes: Vector[SharedWrite],
      methods: Map[String, Method],
      library: Int
  ) {
    def verdicts: Map[String, Verdict] = methods.view.mapValues(_.verdict).toMap
  }

  /** How many authored methods are pure, impure and unknown, and what most often leaves a method unknown. */
  def summary(model: Model): Vector[String] = {
    val verdicts = model.verdicts.values.toVector
    val effects  = verdicts.collect { case Verdict.Effectful(_, certainty) => certainty }
    val unknown  = effects.collect { case Certainty.Unknown(origin) => origin }
    val impure   = effects.size - unknown.size
    val tally    = s"purity: ${verdicts.count(_ == Verdict.Pure)} pure, $impure impure, ${unknown.size} unknown of " +
      s"${verdicts.size} methods, reading ${model.library} library methods"
    tally +: unknown.groupMapReduce(identity)(_ => 1)(_ + _).toVector.sortBy((cause, count) => (-count, cause))
      .take(Shown).map((cause, count) => s"purity: unknown $count: $cause")
  }

  private val Shown = 25

  private val Contracted = Set(Kind.Writes, Kind.Output, Kind.Input, Kind.Random)
  private val TupleApply = "Tuple[0-9]+\\.apply".r
  // Calls that traverse a view only until they find their answer, as a loop that breaks does.
  private val Stopping =
    Set("collectFirst", "find", "exists", "forall", "contains", "indexWhere", "indexOf", "head", "headOption")

  def findings(model: Model, nodes: Map[String, Node]): Vector[Finding] =
    model.discards.flatMap(discard =>
      Finding.at(
        nodes,
        discard.node,
        Finding.Rule.PureDiscard,
        s"${discard.callee} has no effects, so dropping its result at ${discard.location} leaves a call that does nothing",
        suppressed = discard.suppressed
      )
    ) ++ model.transforms.flatMap(transform =>
      Finding.at(
        nodes,
        transform.node,
        Finding.Rule.EffectfulTransform,
        s"the function passed to ${transform.operation} at ${transform.location} ${transform.effect}; " +
          (if (transform.view) "a view runs it again on every traversal, so perform effects in foreach or a loop"
           else "keep transformations pure and perform effects in foreach or a loop"),
        suppressed = transform.suppressed
      )
    ) ++ model.methods.toVector.sortBy(_._1).flatMap((name, method) =>
      method.role.flatMap(role =>
        method.effects.find(effect =>
          // Writes on the receiver or in library bodies are caches far more often than not.
          Contracted(effect.kind) &&
            !(effect.kind == Kind.Writes && (effect.receiver || effect.library))
        ).flatMap(effect =>
          Finding.at(
            nodes,
            method.node,
            Finding.Rule.ImpureContract,
            s"${Finding.short(name)} ${effect.shown}; collections, sorting and pattern matching call $role any " +
              "number of times and assume it has no effects",
            suppressed = method.suppressed
          )
        )
      )
    ) ++ model.writes.flatMap(write =>
      Finding.at(
        nodes,
        write.node,
        Finding.Rule.SharedWrite,
        s"the function ${write.entry} runs at ${write.location} assigns ${write.variable}, which other threads share " +
          "without synchronisation; use an atomic or concurrent type, or synchronized",
        suppressed = write.suppressed
      )
    )

  /** Methods `locked` records as pure that no longer are. */
  def regressions(model: Model, nodes: Map[String, Node], locked: Set[String]): Vector[Finding] =
    locked.toVector.sorted.flatMap(name =>
      model.methods.get(name).flatMap(method =>
        (method.verdict match {
          case Verdict.Pure                => None
          case Verdict.Effectful(cause, _) => Some(cause)
        }).flatMap(cause =>
          Finding.at(
            nodes,
            method.node,
            Finding.Rule.PurityRegression,
            s"${Finding.short(name)} was pure and now $cause; restore it, or record the change with " +
              "bonsaiPurityWrite",
            suppressed = method.suppressed
          )
        )
      )
    )

  /** The lock recording the pure methods of main code, one qualified name per line. */
  def lock(model: Model, nodes: Map[String, Node]): String = model.methods.toVector.collect {
    case (name, method) if method.verdict == Verdict.Pure && nodes.get(method.node).exists(!_.test) => name
  }.sorted.mkString("", "\n", "\n")

  /** Methods of main code with an effect the most specific policy covering them does not allow; an unknown effect
    * counts as one.
    */
  def violations(model: Model, nodes: Map[String, Node], policies: Vector[Policy]): Vector[Finding] =
    model.methods.toVector.sortBy(_._1).view.filter((_, method) => nodes.get(method.node).exists(!_.test)).flatMap {
      (name, method) =>
        policies.flatMap(policy => policy.covers(name).map(_ -> policy)).maxByOption(_._1).flatMap((_, policy) =>
          method.effects.find(effect => effect.kind != Kind.Throws && !policy.allowed(effect.kind)).flatMap(effect =>
            Finding.at(
              nodes,
              method.node,
              Finding.Rule.EffectPolicy,
              s"${Finding.short(name)} ${effect.shown}; the effects policy for ${policy.prefix} allows " +
                (if (policy.allowed.isEmpty) "no effects"
                 else
                   s"only ${Policy.Kinds.collect { case (id, kind) if policy.allowed(kind) => id }.toSeq.sorted
                       .mkString(", ")}"),
              suppressed = method.suppressed
            )
          )
        )
    }.toVector

  /** A set of small non-negative integers, sorted. */
  private final class Ids(val values: Array[Int]) {
    def isEmpty: Boolean              = values.isEmpty
    def foreach(f: Int => Unit): Unit = {
      var i = 0
      while (i < values.length) { f(values(i)); i += 1 }
    }
    def exists(f: Int => Boolean): Boolean = values.exists(f)
    def forall(f: Int => Boolean): Boolean = values.forall(f)

    def filter(f: Int => Boolean): Ids = {
      var i = 0
      while (i < values.length && f(values(i))) i += 1
      if (i == values.length) this
      else {
        val kept = new Array[Int](values.length - 1)
        System.arraycopy(values, 0, kept, 0, i)
        var n = i
        i += 1
        while (i < values.length) {
          if (f(values(i))) {
            kept(n) = values(i)
            n += 1
          }
          i += 1
        }
        Ids(java.util.Arrays.copyOf(kept, n))
      }
    }

    def ++(other: Ids): Ids =
      if (other.isEmpty) this
      else if (isEmpty) other
      else Ids.union(values, other.values)

    override def equals(other: Any): Boolean = other match {
      case ids: Ids => java.util.Arrays.equals(values, ids.values)
      case _        => false
    }

    override def hashCode: Int = java.util.Arrays.hashCode(values)
  }

  private object Ids {
    val Empty: Ids = Ids(Array.emptyIntArray)

    private def union(left: Array[Int], right: Array[Int]): Ids = {
      val merged    = new Array[Int](left.length + right.length)
      var (i, j, n) = (0, 0, 0)
      while (i < left.length || j < right.length) {
        val next =
          if (j == right.length || i < left.length && left(i) <= right(j)) {
            val value = left(i)
            i += 1
            if (j < right.length && right(j) == value) j += 1
            value
          } else {
            val value = right(j)
            j += 1
            value
          }
        merged(n) = next
        n += 1
      }
      Ids(if (n == merged.length) merged else java.util.Arrays.copyOf(merged, n))
    }

    /** The members of `now` that `before` lacks, when `now` holds all of `before`. */
    def gained(before: Ids, now: Ids): Option[Ids] = {
      val (old, current) = (before.values, now.values)
      val added          = new Array[Int]((current.length - old.length).max(0))
      var (i, j, k)      = (0, 0, 0)
      var contained      = old.length <= current.length
      while (contained && j < current.length)
        if (i < old.length && old(i) == current(j)) {
          i += 1
          j += 1
        } else if (i < old.length && old(i) < current(j)) contained = false
        else if (k < added.length) {
          added(k) = current(j)
          k += 1
          j += 1
        } else contained = false
      Option.when(contained && i == old.length && k == added.length)(Ids(added))
    }

    /** Collects members in any order, with repeats. */
    final class Builder {
      private var members = new Array[Int](16)
      private var count   = 0

      def add(id: Int): this.type = {
        if (count == members.length) members = java.util.Arrays.copyOf(members, count * 2)
        members(count) = id
        count += 1
        this
      }

      def clear(): this.type = {
        count = 0
        this
      }

      def addAll(ids: Ids): this.type = {
        val size = ids.values.length
        if (count + size > members.length)
          members = java.util.Arrays.copyOf(members, math.max(count + size, members.length * 2))
        System.arraycopy(ids.values, 0, members, count, size)
        count += size
        this
      }

      def result(): Ids =
        if (count == 0) Empty
        else {
          val sorted = java.util.Arrays.copyOf(members, count)
          java.util.Arrays.sort(sorted)
          var distinct = 1
          var i        = 1
          while (i < count) {
            if (sorted(i) != sorted(distinct - 1)) {
              sorted(distinct) = sorted(i)
              distinct += 1
            }
            i += 1
          }
          Ids(if (distinct == count) sorted else java.util.Arrays.copyOf(sorted, distinct))
        }
    }
  }

  /** Effect inference over the bodies of the build's methods and class templates. `implementations` gives the bodies a
    * virtual call can reach, or nothing when one lies outside the build, and `concurrent` names, beside the standard
    * ones, the methods that run the functions passed to them on other threads.
    */
  class Inference(
      bodies: collection.Map[Symbol, tpd.DefDef],
      templates: collection.Map[Symbol, tpd.Template],
      implementations: Symbol => Option[Vector[Symbol]],
      concurrent: Set[String]
  )(using Context) {

    private type Effect = net.kurobako.bonsai.effects.Effect[Symbol]

    private def plain(kind: Kind, cause: String, target: Option[Symbol] = None): Effect =
      Effect(kind, cause, target = target)

    private def rank(effect: Effect): (Int, String, Boolean, Boolean, String) =
      (Priority.indexOf(effect.kind), effect.shown, effect.onThis, effect.library, effect.cause)

    // Effects are compared and joined by number, as summaries hold thousands of them.
    private val effectIds   = mutable.HashMap.empty[Effect, Int]
    private val effectTable = mutable.ArrayBuffer.empty[Effect]
    private val targeted    = java.util.BitSet()
    private val retargets   = mutable.LongMap.empty[Int]
    private val viewings    = mutable.LongMap.empty[Int]
    private val viewKinds   = mutable.HashMap.empty[(String, Boolean), Int]

    private def intern(effect: Effect): Int = effectIds.get(effect).getOrElse {
      val id = effectTable.size
      effectTable += effect
      if (effect.target.isDefined) targeted.set(id)
      effectIds(effect) = id
      id
    }

    private def interned(effects: Iterable[Effect]): Ids = {
      val ids = Ids.Builder()
      effects.foreach(effect => ids.add(intern(effect)))
      ids.result()
    }

    // Builders nest with the summaries being made, so the finished ones are kept for the next.
    private val spares = mutable.Stack.empty[Ids.Builder]

    private def builder(): Ids.Builder = if (spares.isEmpty) Ids.Builder() else spares.pop().clear()

    private def spare(finished: Ids.Builder): Unit = spares.push(finished)

    private def localTarget(id: Int, owner: Symbol): Boolean = {
      val target = effectTable(id).target
      target.isDefined && local(target.get, owner)
    }

    private def decoded(ids: Ids): Vector[Effect] = ids.values.iterator.map(effectTable(_)).toVector

    private def retarget(id: Int, target: Option[Symbol]): Int =
      retargets.getOrElseUpdate(
        id.toLong << 32 | target.fold(0L)(_.id + 1L),
        intern(effectTable(id).copy(onThis = false, target = target))
      )

    private enum Form {
      case Opaque, Plain
    }

    private enum Argument {
      case Function(facts: Facts)
      case Parameter(index: Int)
      case Expression(tree: tpd.Tree, form: Form)
    }

    /** What a call runs: the bodies of `targets`, code an axiom describes, the body of `method` the object `reference`
      * takes it from decides, or `otherwise` when that object is unknown, or a function value.
      */
    private enum Resolution {
      case Bodies(targets: Vector[Symbol])
      case Axiom(summary: Summary)
      case Dispatch(method: Symbol, reference: tpd.Tree, otherwise: Resolution)
      case Invocation(function: tpd.Tree)
    }

    private case class Call(
        name: String,
        resolution: Resolution,
        receiver: Receiver[Symbol],
        arguments: Vector[Argument]
    ) {
      // Constructions are dropped to validate their arguments.
      def constructs: Boolean = name.endsWith("<init>")
    }

    /** What evaluating a tree does, before the summaries of its callees are known; each is gathered once, and compares
      * by identity.
      */
    private class Facts(val effects: Vector[Effect], val invokes: Set[Int], val calls: Vector[Call]) {
      val ids: Ids = interned(effects)
    }

    /** What an expression evaluates to, as far as calls on it and through it go: an object of an exact class with the
      * values it captured, a function with the frame it closed over, a parameter of the method summarised or of one
      * enclosing it, any of several values, a value with no behaviour of its own, or one the inference cannot follow.
      */
    private enum Value {
      case Object(cls: Symbol, bindings: Map[Symbol, Value])
      case Function(facts: Facts, frame: Frame)
      case Parameter(symbol: Symbol)
      case Several(values: Set[Value])
      case Inert
      case Opaque

      // Values nest, and are hashed whole at every lookup of a frame or a key that holds them.
      override lazy val hashCode: Int = this match {
        case product: (Object | Function | Parameter | Several) => ScalaRunTime._hashCode(product)
        case _                                                  => ordinal
      }

      // How many objects and functions deep the value goes.
      lazy val levels: Int = this match {
        case Object(_, bindings)                   => 1 + bindings.valuesIterator.map(_.levels).maxOption.getOrElse(0)
        case Function(_, Frame(_, self, bindings)) =>
          1 + (self.iterator ++ bindings.valuesIterator).map(_.levels).maxOption.getOrElse(0)
        case Several(values) => values.iterator.map(_.levels).maxOption.getOrElse(0)
        case _               => 0
      }
    }

    /** Where evaluation happens: `root` numbers the parameters, `self` is this, and `bindings` holds the known values
      * of parameters and captured symbols.
      */
    private case class Frame(root: Symbol, self: Option[Value], bindings: Map[Symbol, Value]) {
      override lazy val hashCode: Int = ScalaRunTime._hashCode(this)
    }

    /** A method summarised for one receiver, or for any when `self` is empty, and for the objects bound to its context
      * parameters and to the values of enclosing methods it captures.
      */
    private case class Key(method: Symbol, self: Option[Value], bindings: Map[Symbol, Value] = Map.empty) {
      // hashed far more often than built
      override val hashCode: Int = scala.util.hashing.MurmurHash3.caseClassHash(this)
    }

    /** A method's effects, the indices of its function and by-name parameters it runs, and the parameters of methods
      * enclosing it it runs.
      */
    private case class Summary(effects: Ids, invokes: Set[Int], captures: Set[Symbol] = Set.empty)

    private object Summary {
      def apply(effects: Set[Effect], invokes: Set[Int]): Summary = Summary(interned(effects), invokes, Set.empty)
    }

    // The keys of lookups that hash far more often than they are built.
    private case class Evaluation(tree: tpd.Tree, frame: Frame) {
      override val hashCode: Int = ScalaRunTime._hashCode(this)
    }

    private case class Returned(target: Symbol, receiver: Option[Value], arguments: Vector[Value]) {
      override val hashCode: Int = ScalaRunTime._hashCode(this)
    }

    private case class Resolving(facts: Facts, frame: Frame, method: Option[Symbol]) {
      override val hashCode: Int = ScalaRunTime._hashCode(this)
    }

    private case class TransformSite(method: Symbol, operation: String, lambda: Facts, tree: tpd.Tree, view: Boolean)
    private case class WriteSite(method: Symbol, variable: Symbol, entry: String, tree: tpd.Tree)
    private case class DiscardSite(method: Symbol, name: String, facts: Facts, tree: tpd.Tree)

    private val summaries     = mutable.Map.empty[Key, Summary]
    private val views         = mutable.Map.empty[Key, (Summary, Summary)]
    private val states        = mutable.HashMap.empty[Key, State]
    private val open          = mutable.ArrayBuffer.empty[Key]
    private val dirtied       = mutable.ArrayBuffer.empty[Key]
    private val readers       = mutable.HashMap.empty[Key, mutable.ArrayBuffer[Key]]
    private val computing     = mutable.Stack.empty[Key]
    private val Silent        = Summary(Ids.Empty, Set.empty, Set.empty)
    private val locals        = mutable.Map.empty[Symbol, tpd.Tree]
    private val lambdas       = mutable.Map.empty[Symbol, Facts]
    private val evaluated     = mutable.Map.empty[Evaluation, Value]
    private val returning     = mutable.Map.empty[Returned, Value]
    private val evaluating    = mutable.Set.empty[Returned]
    private val pendingValues = mutable.Set.empty[Evaluation]
    private val sites         = mutable.Map.empty[Symbol, mutable.ArrayBuffer[(Symbol, Vector[Argument])]]
    private val tracing       = mutable.Set.empty[Symbol]
    private val running       = mutable.Set.empty[Symbol]
    private val resolving     = mutable.Set.empty[Resolving]
    private val pending       = mutable.Queue.empty[(Symbol, Int)]
    private val explored      = mutable.Set.empty[Symbol]
    private val facts         = mutable.Map.empty[Symbol, Facts]
    private val fresh         = mutable.Set.empty[Symbol]
    private val transforms    = mutable.ArrayBuffer.empty[TransformSite]
    // View transformations a strict call traverses once, and whether that call may stop before the end.
    private val consumed = mutable.Map.empty[Span, Boolean]
    private val discards = mutable.ArrayBuffer.empty[DiscardSite]
    private val writes   = mutable.ArrayBuffer.empty[WriteSite]

    // Methods that run the functions passed to them on other threads.
    private val Spawning = Set(
      "scala.concurrent.Future.apply",
      "scala.concurrent.Future.onComplete",
      "scala.concurrent.Future.foreach",
      "scala.concurrent.ExecutionContext.execute",
      "java.util.concurrent.Executor.execute",
      "java.util.concurrent.ExecutorService.submit",
      "java.util.concurrent.CompletableFuture.supplyAsync",
      "java.util.concurrent.CompletableFuture.runAsync",
      "java.lang.Thread.<init>",
      "java.lang.Thread.startVirtualThread",
      "java.lang.Thread.Builder.start",
      "java.lang.Thread.Builder.unstarted"
    )

    private val Transformations = Set(
      "map",
      "flatMap",
      "filter",
      "filterNot",
      "withFilter",
      "collect",
      "collectFirst",
      "exists",
      "forall",
      "count",
      "find",
      "partition",
      "partitionMap",
      "groupBy",
      "groupMap",
      "groupMapReduce",
      "sortBy",
      "sortWith",
      "maxBy",
      "minBy",
      "takeWhile",
      "dropWhile",
      "span",
      "distinctBy"
    )
    private val View          = requiredClass("scala.collection.View")
    private val IteratorClass = requiredClass("scala.collection.Iterator")
    private val traversals    = Traversals()

    /** How many library bodies the inference read. */
    def library: Int = explored.size

    /** Infers every summary, settling recursion at the least fixpoint, from which effects only grow. */
    def run(): Unit = {
      bodies.foreach((method, definition) => facts(method) = Walker(method, method).facts(definition.rhs))
      templates.foreach((cls, template) =>
        facts(cls.primaryConstructor) = Walker(cls.primaryConstructor, cls.primaryConstructor).facts(template)
      )
      // Library bodies the build reaches are read in turn, and may reach further ones.
      while (pending.nonEmpty) {
        val (method, depth) = pending.dequeue()
        facts(method) =
          try
            treeOf(method) match {
              case definition: tpd.DefDef => Walker(method, method, depth).facts(definition.rhs)
              case _                      => Facts(Vector(plain(Kind.Unknown, "has no body")), Set.empty, Vector.empty)
            }
          catch {
            case NonFatal(error) =>
              Facts(Vector(plain(Kind.Unknown, s"could not be read: $error")), Set.empty, Vector.empty)
          }
      }
      facts.keys.toVector.foreach(method => solve(Key(method, None)))
    }

    /** Where a key stands in the search for strongly connected components of the keys that read each other: `index` is
      * the order it was reached in, `low` the earliest key still unsettled that it reaches, and `position` its place
      * among the unsettled keys.
      */
    private final class State(val index: Int, val position: Int) {
      var low: Int         = index
      var settled: Boolean = false
      var dirty: Boolean   = false
    }

    // A key is computed once its callees have settled, and computed again only while it is in a cycle with a callee
    // that has grown since, so the keys settle one component at a time, callees first. Summaries only grow, so each
    // component stops at its least fixpoint.
    private def solve(key: Key): State = states.getOrElse(key, null) match {
      case null  => visit(key)
      case state => state
    }

    private def visit(key: Key): State = {
      val state = State(states.size, open.size)
      val mark  = dirtied.size
      states(key) = state
      open += key
      isolated {
        evaluate(key)
        if (state.low == state.index) {
          var next = mark
          while (next < dirtied.size) {
            val stale = dirtied(next)
            next += 1
            states(stale).dirty = false
            evaluate(stale)
          }
        }
      }
      if (state.low == state.index) {
        dirtied.remove(mark, dirtied.size - mark)
        open.view.drop(state.position).foreach { member =>
          states(member).settled = true
          readers -= member
        }
        open.dropRightInPlace(open.size - state.position)
      }
      state
    }

    // A key is computed as if it were the first, whichever key's computation reached it.
    private def isolated(body: => Unit): Unit = {
      val (resolved, ran, returned, valued) =
        (resolving.toVector, running.toVector, evaluating.toVector, pendingValues.toVector)
      resolving.clear()
      running.clear()
      evaluating.clear()
      pendingValues.clear()
      try body
      finally {
        resolving.clear()
        running.clear()
        evaluating.clear()
        pendingValues.clear()
        resolving ++= resolved
        running ++= ran
        evaluating ++= returned
        pendingValues ++= valued
      }
    }

    private def evaluate(key: Key): Unit = {
      computing.push(key)
      val next =
        try compute(key)
        finally computing.pop(): Unit
      if (summaries.get(key).fold(next != Silent)(_ != next)) {
        summaries(key) = next
        readers.getOrElse(key, Nil).foreach { reader =>
          val state = states(reader)
          if (!state.dirty) {
            state.dirty = true
            dirtied += reader
          }
        }
      }
    }

    private def compute(key: Key): Summary = factsOf(key.method).fold(
      if (field(key.method)) Summary(Set.empty, Set.empty)
      else Summary(Set(plain(Kind.Unknown, s"calls ${short(key.method)}")), Set.empty)
    )(known => resolve(known, frame(key.method, key.self, key.bindings), Some(key.method)))

    // Reading an immutable field runs nothing; its initialiser ran with its object.
    private def field(member: Symbol): Boolean = !member.isOneOf(Flags.Method | Flags.Lazy | Flags.Mutable)

    private def frame(method: Symbol, self: Option[Value], bindings: Map[Symbol, Value] = Map.empty): Frame = Frame(
      method,
      self.orElse(Option.when(method.owner.is(Flags.ModuleClass))(Value.Object(method.owner, Map.empty))),
      self.fold(Map.empty[Symbol, Value]) {
        case Value.Object(_, captured) => captured
        case _                         => Map.empty
      } ++ bindings
    )

    /** The facts of a method of the build or a library, reading a library body on first need within the limits. */
    private def factsOf(method: Symbol): Option[Facts] = facts.get(method).orElse(
      Option.when(explored.size < MaximumLibrary)(treeOf(method)).collect {
        case definition: tpd.DefDef if !definition.rhs.isEmpty =>
          explored += method
          val read =
            try Walker(method, method, 1).facts(definition.rhs)
            catch {
              case NonFatal(_) =>
                Facts(Vector(plain(Kind.Unknown, s"could not read $method")), Set.empty, Vector.empty)
            }
          facts(method) = read
          read
      }
    )

    private def summary(key: Key): Summary = {
      val state = solve(key)
      if (!state.settled) computing.headOption.foreach { reader =>
        val own = states(reader)
        own.low = math.min(own.low, state.low)
        readers.getOrElseUpdate(key, mutable.ArrayBuffer.empty) += reader
      }
      summaries.getOrElse(key, Silent)
    }

    /** Dropped calls with no effects at all, or that only read as far as is known, by enclosing method, with the
      * callee's name and the call.
      */
    def pointless: Vector[(Symbol, String, tpd.Tree)] = discards.toVector.collect {
      case site
          if {
            val summary = resolve(site.facts, frame(site.method, None), None, keepFresh = true)
            !runsUnknown(summary) && summary.effects.forall(id =>
              effectTable(id).kind == Kind.Reads && !effectTable(id).assumed
            )
          } =>
        (site.method, site.name, site.tree)
    }

    /** Element-wise transformations of collections whose functions themselves write state, perform output, or are
      * nondeterministic, with the first such effect; reading state or input there runs once per element and hides
      * nothing. Effects that only come through the build's methods they call are often caches, and folds are loops that
      * may stop early, which foreach cannot; neither counts here.
      */
    def effectful: Vector[(Symbol, String, String, tpd.Tree, Boolean)] = transforms.toVector.flatMap { site =>
      val once = consumed.get(site.tree.span).filter(_ => site.view)
      if (once.contains(true)) None
      else
        decoded(resolve(site.lambda, frame(site.method, None), None).effects).filter(effect =>
          Observable(effect.kind) && effect.via.isEmpty
        ).minOption.map(effect => (site.method, site.operation, effect.shown, site.tree, site.view && once.isEmpty))
    }

    /** Whether calling `lambda`, written in `method`, has no effect at all, not even throwing, and runs nothing that is
      * unknown.
      */
    def silent(method: Symbol, lambda: tpd.DefDef): Boolean =
      lambdas.get(lambda.symbol).exists(facts => quiet(resolve(facts, frame(method, None), None)))

    /** Whether calling `method` has no effect at all, not even throwing, and runs nothing that is unknown. */
    def silent(method: Symbol): Boolean = summaries.get(Key(method, None)).exists(quiet)

    private def runsUnknown(summary: Summary): Boolean = summary.invokes.nonEmpty || summary.captures.nonEmpty

    private def quiet(summary: Summary): Boolean = summary.effects.isEmpty && !runsUnknown(summary)

    /** Assignments to shared variables by functions other threads run, by enclosing method, with the variable, the
      * method running the function and the assignment.
      */
    def shared: Vector[(Symbol, String, String, tpd.Tree)] =
      writes.toVector.map(site => (site.method, site.variable.name.toString, site.entry, site.tree))

    private val Priority =
      Vector(Kind.Output, Kind.Input, Kind.Random, Kind.Writes, Kind.Reads, Kind.Unknown, Kind.Throws)
    private val Observable = Set(Kind.Writes, Kind.Output, Kind.Random)

    private given Ordering[Effect] = Ordering.by(rank)

    private val ranked = mutable.Map.empty[Symbol, Vector[Effect]]

    private def ordered(method: Symbol): Vector[Effect] = ranked.getOrElseUpdate(
      method,
      summaries.get(Key(method, None)).fold(Vector.empty)(summary =>
        decoded(summary.effects).map(effect => rank(effect) -> effect).sortBy(_._1).map(_._2)
      )
    )

    /** Each kind of effect `method` has, by its first cause, apart on its receiver alone and elsewhere, and in library
      * bodies and the build.
      */
    def observed(method: Symbol): Vector[Purity.Observed] = ordered(method).map(effect =>
      Purity.Observed(effect.kind, effect.shown, effect.onThis, effect.library)
    ).distinctBy(effect => (effect.kind, effect.receiver, effect.library))

    private val Comparisons =
      Set("scala.math.Ordering", "scala.math.Ordered", "java.util.Comparator", "java.lang.Comparable")

    /** The contract `method` fulfils for collections, sorting and pattern matching, if any. */
    def role(method: Symbol): Option[String] = {
      val name       = method.name.toString
      val overridden = method.allOverriddenSymbols.toVector
      Option.when(
        overridden.exists(other => other == defn.Any_equals || other == defn.Any_hashCode) ||
          Set("compare", "compareTo")(name) && overridden.exists(other => Comparisons(qualified(other.owner))) ||
          Set("unapply", "unapplySeq")(name) && method.owner.is(Flags.ModuleClass)
      )(name)
    }

    /** What is known of `method`: impure when any effect besides throwing is proven, unknown when one may hide. */
    def verdict(method: Symbol): Purity.Verdict = {
      val effects = ordered(method)
      effects.find(effect => effect.kind != Kind.Throws && effect.kind != Kind.Unknown).map(effect =>
        Purity.Verdict.Effectful(effect.shown, Purity.Certainty.Known)
      ).orElse(effects.find(_.kind == Kind.Unknown).map(effect =>
        Purity.Verdict.Effectful(effect.shown, Purity.Certainty.Unknown(effect.cause))
      )).getOrElse(Purity.Verdict.Pure)
    }

    /** The summary of `facts` evaluated in `frame`; effects on locals of `method` stay inside it. `keepFresh` keeps
      * those on values the enclosing method allocated, to ask whether evaluating `facts` does anything at all; values a
      * function passed along allocates die with it.
      */
    private def resolve(facts: Facts, frame: Frame, method: Option[Symbol], keepFresh: Boolean = false): Summary = {
      def summarise(): Summary = {
        val effects  = builder().addAll(facts.ids)
        val invokes  = mutable.Set.from(facts.invokes)
        val captures = mutable.Set.empty[Symbol]
        facts.calls.foreach { call =>
          val called = ran(call.resolution, call.arguments, frame)
          captures ++= called.captures
          call.receiver match {
            case Receiver.This => effects.addAll(called.effects)
            case receiver      =>
              called.effects.foreach { id =>
                if (!effectTable(id).onThis) effects.add(id)
                else
                  receiver match {
                    case Receiver.Fresh         => if (keepFresh) effects.add(id)
                    case Receiver.Other(target) => effects.add(retarget(id, target))
                    case Receiver.This          => ()
                  }
              }
          }
          // A function value's invocations are already this frame's, where a callee's name its own parameters.
          if (call.resolution.isInstanceOf[Resolution.Invocation]) invokes ++= called.invokes
          else called.invokes.foreach(index =>
            call.arguments.lift(index) match {
              case Some(Argument.Function(inner)) =>
                val nested = resolve(inner, frame, method)
                effects.addAll(nested.effects)
                invokes ++= nested.invokes
                captures ++= nested.captures
              case Some(Argument.Parameter(parameter))          => invokes += parameter
              case Some(Argument.Expression(tree, Form.Opaque)) =>
                val ran = invoke(evaluate(tree, frame), frame)
                effects.addAll(ran.effects)
                invokes ++= ran.invokes
                captures ++= ran.captures
              case _ => ()
            }
          )
        }
        // A captured parameter is this method's own once evaluation reaches the method declaring it.
        val own                 = positions(frame.root)
        val (outer, unenclosed) = captures.toSet.filterNot(own.contains)
          .partition(parameter => ownedBy(frame.root, parameter.owner))
        invokes ++= captures.flatMap(own.get)
        // Another method's parameter runs whatever the build passes it.
        unenclosed.filterNot(running).foreach { parameter =>
          running += parameter
          val ran =
            try invoke(passed(parameter), frame)
            finally running -= parameter
          effects.addAll(ran.effects)
          invokes ++= ran.invokes
        }
        val found = effects.result()
        spare(effects)
        Summary(
          method.fold(found)(owner =>
            found.filter(id => !targeted.get(id) || !localTarget(id, owner))
          ),
          invokes.toSet,
          outer
        )
      }
      val key = Resolving(facts, frame, method)
      // Evaluating the same facts in the same frame again from within itself never ends, so what cannot be known stays
      // unknown.
      if (!resolving.add(key)) Summary(Set(plain(Kind.Unknown, "runs a function that runs itself")), Set.empty)
      else
        try summarise()
        finally resolving -= key
    }

    private def ran(resolution: Resolution, arguments: Vector[Argument], frame: Frame): Summary = resolution match {
      case Resolution.Bodies(targets) =>
        joined(targets.map(target => Key(target, None, bound(target, target, arguments, frame))))
      case Resolution.Axiom(summary)                         => summary
      case Resolution.Dispatch(method, reference, otherwise) =>
        dispatched(method, Some(reference), arguments, frame).getOrElse(ran(otherwise, arguments, frame))
      case Resolution.Invocation(function) => invoke(evaluate(function, frame), frame)
    }

    @annotation.tailrec
    private def bodies(resolution: Resolution): Vector[Symbol] = resolution match {
      case Resolution.Bodies(targets)           => targets
      case Resolution.Dispatch(_, _, otherwise) => bodies(otherwise)
      case _                                    => Vector.empty
    }

    /** The bodies a call through `reference`, or on this, runs, chosen by the objects it may be taken from; nothing
      * when one of them is unknown.
      */
    private def dispatched(
        method: Symbol,
        reference: Option[tpd.Tree],
        arguments: Vector[Argument],
        frame: Frame
    ): Option[Summary] =
      objects(reference.fold(frame.self)(holder(_, frame)).getOrElse(Value.Opaque)).orElse(reference.flatMap(typed(_)))
        .flatMap { candidates =>
          // A module is always itself, as its methods are summarised without a receiver.
          val targets = candidates.map { candidate =>
            val target = matching(method, candidate.cls)
            Key(
              target,
              Option.unless(candidate.cls.is(Flags.ModuleClass) && candidate.bindings.isEmpty)(candidate),
              bound(target, method, arguments, frame)
            )
          }
          Option.when(targets.nonEmpty &&
            targets.forall(key => key.method.exists && (field(key.method) || factsOf(key.method).isDefined)))(
            joined(targets)
          )
        }

    // The summary of a body as its callers see it, each effect naming the body it comes from, kept while the summary
    // it was made from stands.
    private def viewed(key: Key): Summary = {
      val part = summary(key)
      views.get(key).filter(_._1 eq part).fold {
        val target  = key.method
        val library = !bodies.contains(target) && !templates.contains(target.owner)
        val name    = short(target)
        val kind    = viewKinds.getOrElseUpdate((name, library), viewKinds.size)
        val via     = Some(name)
        // A summary only grows, so what it gained since the last view is all that needs viewing.
        val (before, gained) = views.get(key).flatMap((earlier, view) =>
          Ids.gained(earlier.effects, part.effects).map(view.effects -> _)
        ).getOrElse(Ids.Empty -> part.effects)
        val seen = Ids.Builder().addAll(before)
        gained.foreach { id =>
          seen.add(viewings.getOrElseUpdate(
            id.toLong << 32 | kind,
            intern(effectTable(id).copy(via = via, library = effectTable(id).library || library))
          ))
        }
        val view = Summary(seen.result(), part.invokes, part.captures)
        views(key) = part -> view
        view
      }(_._2)
    }

    // The summaries of the bodies a call may run.
    private def joined(keys: Vector[Key]): Summary = keys match {
      case Vector(key) => viewed(key)
      case _           => combined(keys.map(viewed(_)))
    }

    private def combined(parts: Vector[Summary]): Summary = Summary(
      {
        val all = builder()
        parts.foreach(part => all.addAll(part.effects))
        val found = all.result()
        spare(all)
        found
      },
      parts.flatMap(_.invokes).toSet,
      parts.flatMap(_.captures).toSet
    )

    /** The objects `target`, run for a call of `method`, sees in its context parameters and in the values it captures
      * from the methods enclosing it, which decide what its own calls on them run.
      */
    private def bound(target: Symbol, method: Symbol, arguments: Vector[Argument], frame: Frame): Map[Symbol, Value] = {
      var bound = Map.empty[Symbol, Value]
      if (arguments.exists(_.isInstanceOf[Argument.Expression])) {
        val declared = terms(method)
        val own      = terms(target)
        arguments.zipWithIndex.foreach {
          case (Argument.Expression(tree, Form.Plain), index)
              if own.isDefinedAt(index) && declared.lift(index).exists(_.isOneOf(Flags.Given | Flags.Implicit)) =>
            val value = evaluate(tree, frame)
            if (exact(value)) bound = bound.updated(own(index), value)
          case _ => ()
        }
      }
      if (target.exists && !target.owner.isClass)
        frame.bindings.foreach { (symbol, value) =>
          if (symbol.owner != target && ownedBy(target.owner, symbol.owner) && exact(value))
            bound = bound.updated(symbol, value)
        }
      bound
    }

    // Whether `ancestor` is `symbol` or one of the symbols enclosing it.
    private def ownedBy(symbol: Symbol, ancestor: Symbol): Boolean = {
      var current = symbol
      while (current.exists && current != ancestor) current = current.owner
      current.exists
    }

    private def constant(condition: tpd.Tree): Option[Boolean] = condition.tpe.widenTermRefExpr match {
      case ConstantType(Constant(value: Boolean)) => Some(value)
      case _                                      => None
    }

    private def terms(method: Symbol): Vector[Symbol] =
      parameters.getOrElseUpdate(method, Shapes.termClauses(method).flatten.toVector)

    private def positions(method: Symbol): Map[Symbol, Int] =
      numbered.getOrElseUpdate(method, terms(method).zipWithIndex.toMap)

    /** The classes of every object of a sealed class, when its descendants end in classes that cannot be extended; one
      * with a local or anonymous descendant lists itself among its children.
      */
    private def leaves(cls: Symbol): Option[Vector[Symbol]] =
      if (!cls.is(Flags.Sealed) || cls.children.contains(cls)) None
      else
        cls.children.map(child => if (child.isTerm) child.moduleClass else child).foldLeft(
          Option(Vector(cls).filterNot(_.isOneOf(Flags.Abstract | Flags.Trait)))
        )((found, child) =>
          found.zip(
            if (child.isOneOf(Flags.Final | Flags.Module | Flags.Case)) Some(Vector(child)) else leaves(child)
          ).map(_ ++ _)
        )

    // The objects a reference may be taken from by its static class alone: of that class when it cannot be extended,
    // or of each class a sealed one ends in.
    @annotation.tailrec
    private def typed(reference: tpd.Tree): Option[Vector[Value.Object]] = reference match {
      case tpd.Select(qualifier, _) =>
        val cls = qualifier.tpe.widen.classSymbol
        if (!cls.exists || !cls.isClass) None
        else if (cls.isOneOf(Flags.Final | Flags.Module | Flags.Case)) Some(Vector(Value.Object(cls, Map.empty)))
        else leaves(cls).map(_.map(Value.Object(_, Map.empty)))
      case tpd.TypeApply(inner, _) => typed(inner)
      case _                       => None
    }

    private def exact(value: Value): Boolean = value match {
      case Value.Object(_, _)    => true
      case Value.Several(values) => values.forall(exact)
      case _                     => false
    }

    // The exact objects a value may be, or nothing when it may be anything else.
    private def objects(value: Value): Option[Vector[Value.Object]] = value match {
      case candidate: Value.Object => Some(Vector(candidate))
      case Value.Several(values)   =>
        values.toVector.map(objects(_)).foldLeft(Option(Vector.empty[Value.Object]))((all, one) =>
          all.zip(one).map(_ ++ _)
        )
      case _ => None
    }

    /** What running a function value does in `frame`, the frame of the method summarised. */
    private def invoke(value: Value, frame: Frame): Summary = value match {
      case Value.Function(body, closed) =>
        val ran    = resolve(body, closed, None)
        val own    = terms(closed.root)
        val passed = ran.invokes.toVector.view.flatMap(own.lift).map(parameter =>
          invoke(closed.bindings.getOrElse(parameter, Value.Parameter(parameter)), frame)
        ).toVector
        Summary(
          passed.foldLeft(ran.effects)((all, one) => all ++ one.effects),
          passed.flatMap(_.invokes).toSet,
          ran.captures ++ passed.flatMap(_.captures)
        )
      case Value.Parameter(parameter) => Summary(Ids.Empty, Set.empty, Set(parameter))
      case Value.Several(values)      => combined(values.toVector.map(invoke(_, frame)))
      case Value.Inert                => Summary(Set.empty, Set.empty)
      case candidate: Value.Object    =>
        dispatched(
          requiredClass("scala.Function1").requiredMethod("apply"),
          None,
          Vector.empty,
          frame.copy(self = Some(candidate))
        )
          .getOrElse(invoke(Value.Opaque, frame))
      case Value.Opaque => Summary(Set(plain(Kind.Unknown, "runs a function value")), Set.empty)
    }

    private val MaximumNesting = 3

    /** What `tree` evaluates to in `frame`, memoised so the same tree in the same frame always gives the same value; a
      * value defined in terms of itself is opaque.
      */
    private def evaluate(tree: tpd.Tree, frame: Frame): Value = {
      val key = Evaluation(tree, frame)
      evaluated.get(key).getOrElse {
        if (pendingValues(key)) Value.Opaque
        else {
          pendingValues += key
          val value =
            try shallow(evaluated1(tree, frame), MaximumNesting)
            finally pendingValues -= key
          evaluated(key) = value
          value
        }
      }
    }

    private def evaluated1(tree: tpd.Tree, frame: Frame): Value =
      Calls.lambda(tree) match {
        case Some(lambda) =>
          Value.Function(
            lambdas.getOrElseUpdate(lambda.symbol, Walker(frame.root, lambda.symbol, 1).facts(lambda.rhs)),
            frame
          )
        case None =>
          Calls.peel(tree) match {
            case tpd.Block(_, expression)    => evaluate(expression, frame)
            case tpd.If(_, positive, other)  => several(Vector(evaluate(positive, frame), evaluate(other, frame)))
            case tpd.Match(_, cases)         => several(cases.map(clause => evaluate(clause.body, frame)).toVector)
            case tpd.This(_)                 => frame.self.getOrElse(Value.Opaque)
            case tpd.Literal(_)              => Value.Inert
            case tpd.SeqLiteral(elements, _) => several(elements.map(evaluate(_, frame)).toVector)
            case tpd.TypeApply(cast @ tpd.Select(value, _), _) if cast.symbol == defn.Any_asInstanceOf =>
              evaluate(value, frame)
            case tpd.TypeApply(reference: (tpd.Ident | tpd.Select), _) => referenced(reference, tree.tpe.widen, frame)
            case application: tpd.Apply                                => applied(application, frame)
            case reference: (tpd.Ident | tpd.Select) => referenced(reference, reference.tpe.widen, frame)
            case _                                   => Value.Opaque
          }
      }

    private def referenced(reference: tpd.Ident | tpd.Select, result: Type, frame: Frame): Value = {
      val symbol = reference.symbol
      val owner  = holder(reference, frame)
      frame.bindings.get(symbol).orElse(owner.collect {
        case Value.Object(_, bindings) if bindings.contains(symbol) => bindings(symbol)
      }).orElse(locals.get(symbol).map(evaluate(_, frame)))
        .orElse(Option.when(
          symbol.is(Flags.Param) && (symbol.owner == frame.root || within(frame.root, symbol.owner))
        )(Value.Parameter(symbol)))
        .getOrElse(
          if (symbol.is(Flags.Module)) Value.Object(symbol.moduleClass, Map.empty)
          else if (symbol.is(Flags.Method))
            returned(
              symbol,
              owner,
              Vector.empty,
              result,
              reference match {
                case tpd.Select(qualifier, _) if carries(qualifier.tpe.widen) => owner.toVector
                case _                                                        => Vector.empty
              }
            )
          else if (!symbol.is(Flags.Mutable)) stable(symbol, owner)
          else Value.Opaque
        )
    }

    // The member of the class of the object `holder` that `symbol` resolves to, or `symbol` itself.
    private def overriding(symbol: Symbol, holder: Option[Value]): Symbol =
      holder.collect { case Value.Object(cls, _) => matching(symbol, cls) }.filter(_.exists)
        .getOrElse(symbol)

    // An immutable field or module value, by its own definition evaluated with the object holding it.
    private def stable(symbol: Symbol, owner: Option[Value]): Value = {
      val holder = owner.filter(_.isInstanceOf[Value.Object])
      val member = overriding(symbol, holder)
      treeOf(member) match {
        case value: tpd.ValDef if !value.rhs.isEmpty => evaluate(value.rhs, frame(member.owner, holder))
        case _ if member.is(Flags.ParamAccessor)     =>
          terms(member.owner.primaryConstructor).find(_.name == member.name).fold(Value.Opaque)(passed(_))
        case _ => Value.Opaque
      }
    }

    /** What the build passes `parameter`, when only the build can call its method: the values of the arguments at every
      * call, followed through the parameters of the callers in turn. A call passing a parameter on to itself adds
      * nothing; any other cycle is opaque.
      */
    private def passed(parameter: Symbol): Value = {
      val method = parameter.owner
      val index  = terms(method).indexOf(parameter)
      if (index < 0 || tracing(parameter) || !closed(method)) Value.Opaque
      else
        sites.get(method).fold(Value.Opaque) { calls =>
          tracing += parameter
          try several(calls.toVector.flatMap((root, arguments) =>
              (arguments.lift(index) match {
                case Some(Argument.Function(body))      => Some(Value.Function(body, frame(root, None)))
                case Some(Argument.Parameter(other))    => terms(root).lift(other).map(Value.Parameter(_))
                case Some(Argument.Expression(tree, _)) => Some(evaluate(tree, frame(root, None)))
                case _                                  => Some(Value.Opaque)
              }).collect { case value if value != Value.Parameter(parameter) => followed(value) }
            ))
          finally tracing -= parameter
        }
    }

    private def followed(value: Value): Value = value match {
      case Value.Parameter(parameter) => passed(parameter)
      case Value.Several(values)      => several(values.toVector.map(followed(_)))
      case other                      => other
    }

    // Only the build calls a method that is private, private to an enclosing scope, or local, or one of such a class.
    private def closed(method: Symbol): Boolean = closures.getOrElseUpdate(
      method,
      method.allOverriddenSymbols.forall(overridden => templates.contains(overridden.owner)) &&
        method.ownersIterator.takeWhile(!_.is(Flags.Package)).exists(owner =>
          owner.is(Flags.Private) || owner.privateWithin.exists || !owner.owner.isClass
        )
    )

    private def applied(application: tpd.Apply, frame: Frame): Value = {
      val (function, arguments) = flatten(application)
      val method                = function.symbol
      val selected              = Calls.selected(function)
      val receiver              = selected.map(select => evaluate(select.qualifier, frame))
      val passed                = arguments.map(evaluate(_, frame)).toVector
      val carrying              = receiver.filter(_ => selected.exists(select => carries(select.qualifier.tpe.widen)))
        .toVector ++ arguments.zip(passed).collect { case (argument, value) if carries(argument.tpe.widen) => value }
      if (method.isConstructor) {
        val cls = method.owner
        // A class declared in a method captures its values; any class captures its constructor's arguments.
        Value.Object(
          cls,
          frame.bindings ++ cls.info.decls.toList.filter(_.is(Flags.ParamAccessor)).zip(passed) ++
            method.paramSymss.flatten.filter(_.isTerm).zip(passed)
        )
      } else if (
        method.name.toString == "apply" && receiver.exists(_ != Value.Opaque) &&
        selected.exists(select => defn.isFunctionType(select.qualifier.tpe.widen))
      ) Value.Opaque
      else if (!method.exists) Value.Opaque
      else returned(method, holder(function, frame), passed, application.tpe.widen, carrying)
    }

    // The object a member is taken from: its qualifier, or for a bare reference, the module its prefix names or this.
    @annotation.tailrec
    private def holder(reference: tpd.Tree, frame: Frame): Option[Value] = reference match {
      case tpd.Select(qualifier, _) => Some(evaluate(qualifier, frame))
      case tpd.TypeApply(inner, _)  => holder(inner, frame)
      case _                        =>
        val module = reference.tpe match {
          case member: TermRef =>
            member.prefix match {
              case prefix: TermRef if prefix.symbol.is(Flags.Module)    => Some(prefix.symbol.moduleClass)
              case prefix: ThisType if prefix.cls.is(Flags.ModuleClass) => Some(prefix.cls)
              case _                                                    => None
            }
          case _ => None
        }
        module.collect {
          case cls
              if !frame.self.exists {
                case Value.Object(own, _) => own.derivesFrom(cls)
                case _                    => false
              } => Value.Object(cls, Map.empty)
        }.orElse(frame.self)
    }

    /** What calling `method` on `receiver` with `arguments` returns: the value its body evaluates to, or for the
      * standard library, the `carrying` values, those of a type that may hold a function, flowing into a `result` that
      * may hold one.
      */
    private def returned(
        method: Symbol,
        receiver: Option[Value],
        arguments: Vector[Value],
        result: Type,
        carrying: Vector[Value]
    ): Value = {
      val target = overriding(method, receiver)
      val key    = Returned(target, receiver.filter(_.isInstanceOf[Value.Object]), arguments)
      if (pure(target.owner) || target.owner.is(Flags.JavaDefined))
        // What may hold a function carries those that flowed into it, including ones the inference cannot follow.
        if (carries(result)) several(carrying) else Value.Opaque
      else if (evaluating(key) || evaluating.size > MaximumNesting * 4) Value.Opaque
      else
        returning.getOrElse(
          key,
          treeOf(target) match {
            case definition: tpd.DefDef
                if !definition.rhs.isEmpty &&
                  (target.isEffectivelyFinal || key.receiver.isDefined || !target.owner.isClass) =>
              evaluating += key
              val parameters = definition.termParamss.flatten.map(_.symbol)
              val result     =
                try evaluate(
                    definition.rhs,
                    Frame(target, key.receiver, frame(target, key.receiver).bindings ++ parameters.zip(arguments))
                  )
                finally evaluating -= key
              returning(key) = result
              result
            case tpd.ValDef(_, _, _) => stable(target, key.receiver)
            case _                   => Value.Opaque
          }
        )
    }

    // Whether a result type may hold a function: a function itself, or a type applied to one.
    private def carries(tpe: Type): Boolean = defn.isFunctionType(tpe.dealias) ||
      tpe.dealias.argInfos.exists(carries(_))

    private def several(values: Vector[Value]): Value = values.flatMap {
      case Value.Several(inner) => inner.toVector
      case other                => Vector(other)
    }.distinct match {
      case Vector(single)                      => single
      case none if none.isEmpty                => Value.Inert
      case many if many.contains(Value.Opaque) => Value.Opaque
      case many                                => Value.Several(many.toSet)
    }

    // A value no deeper than `depth` objects, so values stay finite as evaluation repeats; a value that is already so
    // is returned as it is.
    private def shallow(value: Value, depth: Int): Value =
      if (value.levels <= depth) value else shallowest(value, depth)

    private def shallowest(value: Value, depth: Int): Value = value match {
      case Value.Object(cls, bindings) =>
        if (depth <= 0) Value.Opaque
        else {
          val inner = shallowed(bindings, depth - 1)
          if (inner eq bindings) value else Value.Object(cls, inner)
        }
      case Value.Function(body, Frame(root, self, bindings)) =>
        if (depth <= 0) Value.Opaque
        else {
          val outer = self.map(shallow(_, depth - 1))
          val inner = shallowed(bindings, depth - 1)
          if (inner.eq(bindings) && outer == self) value else Value.Function(body, Frame(root, outer, inner))
        }
      case Value.Several(values) =>
        val inner = values.map(shallow(_, depth))
        if (inner == values) value else Value.Several(inner)
      case other => other
    }

    private def shallowed(bindings: Map[Symbol, Value], depth: Int): Map[Symbol, Value] =
      if (bindings.valuesIterator.forall(value => shallow(value, depth) eq value)) bindings
      else bindings.view.mapValues(shallow(_, depth)).toMap

    /** The method an application calls, and its arguments across every argument list. */
    private def flatten(tree: tpd.Tree): (tpd.Tree, List[tpd.Tree]) = tree match {
      case tpd.Apply(function, arguments) =>
        val (inner, earlier) = flatten(function)
        (inner, earlier ++ arguments)
      case tpd.TypeApply(function, _) => flatten(function)
      case other                      => (other, Nil)
    }

    private val trees = mutable.HashMap.empty[Symbol, tpd.Tree]

    private def treeOf(symbol: Symbol): tpd.Tree = trees.getOrElseUpdate(symbol, symbol.defTree)

    private val MaximumDepth   = 8
    private val MaximumLibrary = 20000

    /** Whether `method` has a library body the inference reads, queueing it on first sight within the limits. */
    private def explore(method: Symbol, depth: Int): Boolean =
      explored(method) || facts.contains(method) || depth <= MaximumDepth && explored.size < MaximumLibrary &&
        (treeOf(method) match {
          case definition: tpd.DefDef if !definition.rhs.isEmpty =>
            explored += method
            pending.enqueue(method -> depth)
            true
          case _ => false
        })

    private val members = mutable.LongMap.empty[Symbol]

    private def matching(method: Symbol, cls: Symbol): Symbol =
      members.getOrElseUpdate(method.id.toLong << 32 | cls.id, method.denot.matchingMember(cls.thisType))

    private val closures = mutable.HashMap.empty[Symbol, Boolean]

    // A local of the method, or of a function inside it, that holds a value it allocated or is a variable.
    private def local(symbol: Symbol, method: Symbol): Boolean =
      !symbol.owner.isClass && (symbol.is(Flags.Mutable) || fresh(symbol)) && ownedBy(symbol.owner, method)

    private def within(symbol: Symbol, owner: Symbol): Boolean =
      symbol.owner == owner || ownedBy(symbol.owner, owner)

    private val shortNames = mutable.Map.empty[Symbol, String]
    private val parameters = mutable.Map.empty[Symbol, Vector[Symbol]]
    private val numbered   = mutable.Map.empty[Symbol, Map[Symbol, Int]]

    private def short(symbol: Symbol): String = shortNames.getOrElseUpdate(symbol, Finding.short(qualified(symbol)))

    private val pureOwners = mutable.HashMap.empty[Symbol, Boolean]

    private def qualified(symbol: Symbol): String = Calls.qualified(symbol)

    private def pure(owner: Symbol): Boolean =
      pureOwners.getOrElseUpdate(owner, Axioms.Pure.exists(qualified(owner).startsWith(_)))

    private val axioms = Calls()

    /** Gathers the facts of one method or function body; `root` numbers the parameters, `scope` owns the locals, and
      * `depth` counts the library calls between it and the build, whose own bodies are at depth zero.
      */
    private class Walker(root: Symbol, scope: Symbol, depth: Int = 0) extends tpd.TreeTraverser {
      private val effects = mutable.ArrayBuffer.empty[Effect]
      private val invokes = mutable.Set.empty[Int]
      private val calls   = mutable.ArrayBuffer.empty[Call]

      private val parameters =
        Shapes.termClauses(root).flatten.zipWithIndex.toMap

      def facts(tree: tpd.Tree): Facts = {
        traverse(tree)
        Facts(effects.toVector, invokes.toSet, calls.toVector)
      }

      override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
        case tpd.DefDef(_, _, _, _) | tpd.TypeDef(_, _) | tpd.Closure(_, _, _) | tpd.Import(_, _) => ()
        case value: tpd.ValDef                                                                    =>
          val symbol = value.symbol
          if (symbol.exists && !symbol.owner.isClass && !symbol.is(Flags.Mutable)) locals(symbol) = value.rhs
          if (Calls.lambda(value.rhs).isEmpty) {
            if (symbol.exists && !symbol.owner.isClass && !symbol.is(Flags.Mutable) && Calls.allocates(value.rhs))
              fresh += symbol
            traverse(value.rhs)
          }
        case tpd.Block(statements, expression) =>
          statements.foreach { statement =>
            val (callsBefore, effectsBefore, invokesBefore) = (calls.size, effects.size, invokes.size)
            traverse(statement)
            // A tuple is dropped to name what must be evaluated, as sbt's task dependencies are.
            dropped(statement).filter(_ =>
              depth == 0 && calls.size > callsBefore && invokes.size == invokesBefore && !calls.last.constructs &&
                !TupleApply.matches(calls.last.name)
            ).foreach(tree =>
              discards += DiscardSite(
                root,
                calls.last.name,
                Facts(effects.drop(effectsBefore).toVector, Set.empty, calls.drop(callsBefore).toVector),
                tree
              )
            )
          }
          traverse(expression)
        case tpd.Match(selector, cases) =>
          // A binder of a whole case is the value matched, narrowed.
          cases.map(_.pat).foreach {
            case binder @ tpd.Bind(_, tpd.Typed(tpd.Ident(_), _) | tpd.Ident(_)) => locals(binder.symbol) = selector
            case _                                                               => ()
          }
          traverseChildren(tree)
        // A branch a constant condition rules out never runs, as debugging switches leave in library code.
        case tpd.If(condition, positive, negative) if constant(condition).isDefined =>
          traverse(condition)
          traverse(if (constant(condition).contains(true)) positive else negative)
        case tpd.Assign(target, value) =>
          effects ++= access(Kind.Writes, target, s"writes ${target.symbol.name}")
          target match {
            case tpd.Select(qualifier, _) => traverse(qualifier)
            case _                        => ()
          }
          traverse(value)
        case application: tpd.Apply                                                               => call(application)
        case select: tpd.Select if select.symbol.is(Flags.Method) && !select.symbol.isConstructor =>
          call(select)
        case reference: tpd.Ident if reference.symbol.is(Flags.Method) => call(reference)
        case reference: (tpd.Ident | tpd.Select)                       =>
          val symbol = reference.symbol
          parameters.get(symbol).filter(_ => symbol.info.isInstanceOf[ExprType]).foreach(invokes += _)
          if (symbol.is(Flags.Mutable)) effects ++= access(Kind.Reads, reference, s"reads ${symbol.name}")
          traverseChildren(tree)
        case _ => traverseChildren(tree)
      }

      /** A statement whose value is dropped: a non-Unit expression, or `val _ = value`. */
      private def dropped(statement: tpd.Tree): Option[tpd.Tree] = statement match {
        case tpd.Match(selector, List(tpd.CaseDef(tpd.Ident(name), tpd.EmptyTree, _))) if name.toString == "_" =>
          Some(selector)
        case _: (tpd.Apply | tpd.Select | tpd.Ident) if !statement.tpe.widen.isRef(defn.UnitClass) => Some(statement)
        case _                                                                                     => None
      }

      // Reading or writing a field of this object, of a value the method allocated, or of anything else.
      private def access(kind: Kind, target: tpd.Tree, cause: String): Option[Effect] = {
        val symbol = target.symbol
        if (!symbol.owner.isClass)
          Option.unless(within(symbol, scope))(plain(kind, cause, Some(symbol)))
        else
          Calls.field(target, kind, cause, allocated)
      }

      private def receiver(qualifier: Option[tpd.Tree]): Receiver[Symbol] =
        Calls.receiver(qualifier, allocated)

      private def allocated(symbol: Symbol): Boolean = fresh(symbol) && within(symbol, scope)

      private def call(tree: tpd.Tree): Unit = {
        val (function, arguments) = flatten(tree)
        val method                = function.symbol
        val qualifier             = Calls.selected(function).map(_.qualifier)
        qualifier.foreach(traverse(_))
        if (method == defn.throwMethod) {
          arguments.foreach(traverse(_))
          effects += plain(Kind.Throws, "throws")
        } else
          qualifier.filter(q => method.name.toString == "apply" && defn.isFunctionType(q.tpe.widen)) match {
            case Some(value) => invoke(value, arguments)
            case None        => direct(tree, function, qualifier, arguments)
          }
      }

      private def direct(
          tree: tpd.Tree,
          function: tpd.Tree,
          qualifier: Option[tpd.Tree],
          arguments: List[tpd.Tree]
      ): Unit = {
        val method     = function.symbol
        val parameters = Shapes.termClauses(method).flatten
        val passed     = arguments.zipAll(parameters, tpd.EmptyTree, method).collect {
          case (argument, parameter) if !argument.isEmpty => classify(argument, parameter)
        }.toVector
        if (depth == 0 && !tree.tpe.widen.derivesFrom(View))
          qualifier.foreach(pipeline(_).foreach(consumed(_) = Stopping(method.name.toString)))
        if (depth == 0 && Transformations(method.name.toString) && transformed(qualifier))
          passed.zip(arguments).foreach {
            case (Argument.Function(lambda), argument) if Calls.lambda(argument).isDefined =>
              transforms += TransformSite(root, method.name.toString, lambda, tree, viewed(qualifier))
            case _ => ()
          }
        if (depth == 0 && (method +: method.allOverriddenSymbols.toVector).exists(spawns(_)))
          passed.zip(arguments).foreach {
            case (Argument.Function(_), argument) =>
              val body = Calls.lambda(argument).getOrElse(argument)
              unguarded(body).headOption.foreach((variable, assignment) =>
                writes += WriteSite(root, variable, short(method), assignment)
              )
            case _ => ()
          }
        calls += resolveCall(function, qualifier, passed, parameters)
      }

      // A function value runs what it evaluates to, which resolving the call works out.
      private def invoke(function: tpd.Tree, arguments: List[tpd.Tree]): Unit = {
        arguments.foreach(traverse(_))
        Calls.peel(function) match {
          case _: (tpd.Ident | tpd.This) => ()
          case other                     => traverse(other)
        }
        calls +=
          Call("a function value", Resolution.Invocation(function), Receiver.Other(None), Vector.empty)
      }

      private def classify(argument: tpd.Tree, parameter: Symbol): Argument =
        Calls.lambda(argument) match {
          case Some(lambda) =>
            Argument.Function(lambdas.getOrElseUpdate(
              lambda.symbol,
              Walker(root, lambda.symbol, depth).facts(lambda.rhs)
            ))
          case None if parameter.exists && parameter.isTerm && parameter.info.isInstanceOf[ExprType] =>
            Argument.Function(Walker(root, scope, depth).facts(argument))
          case None =>
            Calls.peel(argument) match {
              case reference: tpd.Ident if parameters.contains(reference.symbol) && callable(reference.symbol.info) =>
                Argument.Parameter(parameters(reference.symbol))
              case _ =>
                traverse(argument)
                Argument.Expression(argument, if (callable(argument.tpe)) Form.Opaque else Form.Plain)
            }
        }

      private def resolveCall(
          function: tpd.Tree,
          qualifier: Option[tpd.Tree],
          passed: Vector[Argument],
          parameters: List[Symbol]
      ): Call = {
        val method = function.symbol
        val name   = short(method)
        val on     = receiver(qualifier)
        val call   = resolved(method, name, on, qualifier, function, passed, parameters)
        if (depth == 0)
          bodies(call.resolution).foreach(target =>
            sites.getOrElseUpdate(target, mutable.ArrayBuffer.empty) += root -> passed
          )
        call
      }

      private def resolved(
          method: Symbol,
          name: String,
          on: Receiver[Symbol],
          qualifier: Option[tpd.Tree],
          function: tpd.Tree,
          passed: Vector[Argument],
          parameters: List[Symbol]
      ): Call =
        if (method.isConstructor) {
          val cls = method.owner
          if (templates.contains(cls)) Call(name, Resolution.Bodies(Vector(cls.primaryConstructor)), on, passed)
          else Call(name, Resolution.Axiom(axiom(method, None, parameters)), on, passed)
        } else
          implementations(method) match {
            case Some(targets) if targets.nonEmpty => Call(name, Resolution.Bodies(targets), on, passed)
            case _                                 =>
              val known = axiom(method, qualifier, parameters)
              // What no axiom covers, a library may still describe in its own body.
              if (!method.exists || !known.effects.exists(effectTable(_).kind == Kind.Unknown))
                Call(name, Resolution.Axiom(known), on, passed)
              else {
                val targets   = library(method, qualifier)
                val otherwise = if (targets.isEmpty) Resolution.Axiom(known) else Resolution.Bodies(targets)
                Call(name, Resolution.Dispatch(method, function, otherwise), on, passed)
              }
          }

      /** The library bodies a call runs: the callee's own when no override can replace it, the implementation in the
        * receiver's class when that class cannot be extended, or those of every class a sealed receiver may be. Bodies
        * further from the build than the limit are not read.
        */
      private def library(method: Symbol, qualifier: Option[tpd.Tree]): Vector[Symbol] = {
        val cls = qualifier.fold(method.owner)(_.tpe.widen.classSymbol)
        // A top-level class counts as effectively final however it is declared, so only the modifier itself decides;
        // a receiver built right there, by a constructor or an implicit conversion, is exactly of its class.
        val exact = qualifier.map(Calls.peel(_)).exists {
          case application: tpd.Apply =>
            val built = Calls.selected(application.fun).fold(application.fun.symbol)(_.symbol)
            built.isConstructor || built.is(Flags.Implicit)
          case _ => false
        }
        val inherits = cls.exists && cls.isClass && cls.derivesFrom(method.owner)
        val targets  =
          if (method.isEffectivelyFinal || !method.owner.isClass) Vector(method)
          else if (inherits && (exact || cls.isOneOf(Flags.Final | Flags.Module | Flags.Case)))
            Vector(method.denot.matchingMember(cls.thisType))
          else if (inherits) leaves(cls).fold(Vector.empty)(_.map(leaf => method.denot.matchingMember(leaf.thisType)))
          else Vector.empty
        val distinct = targets.distinct
        if (
          distinct.nonEmpty && distinct.forall(target => target.exists && (field(target) || explore(target, depth + 1)))
        )
          distinct
        else Vector.empty
      }

      private def axiom(method: Symbol, qualifier: Option[tpd.Tree], parameters: List[Symbol]): Summary = {
        val modelled = modelledEffects(method, qualifier, parameters)
        if (
          modelled.effects.isEmpty && method.is(Flags.JavaDefined) && !method.isConstructor &&
          method.hasAnnotation(defn.ThrowsAnnot)
        )
          modelled.copy(effects = interned(Set(plain(Kind.Throws, s"calls ${qualified(method.owner)}.${method.name}"))))
        else modelled
      }

      // The standard library's effects, decided by the receiver for methods mutable and immutable collections share.
      private def modelledEffects(method: Symbol, qualifier: Option[tpd.Tree], parameters: List[Symbol]): Summary = {
        val runs =
          parameters.zipWithIndex.collect { case (parameter, index) if callable(parameter.info) => index }.toSet
        val modelled = Axioms.summary(axioms.of(method, qualifier), runs)
        Summary(modelled.effects.toSet, modelled.invokes)
      }

      private def callable(tpe: Type): Boolean = tpe.isInstanceOf[ExprType] || defn.isFunctionType(tpe.widen)

      // Collections and iterators; for comprehensions over options and results sequence effects by design.
      private def viewed(qualifier: Option[tpd.Tree]): Boolean =
        qualifier.exists(q => Calls.unconverted(q).tpe.widen.derivesFrom(View))

      /** The view transformations a strict call on `tree` runs in its one traversal. */
      private def pipeline(tree: tpd.Tree): List[Span] = Calls.unconverted(tree) match {
        case application: tpd.Apply if application.tpe.widen.derivesFrom(View) =>
          application.span :: Calls.selected(application.fun).fold(Nil)(select => pipeline(select.qualifier))
        case _ => Nil
      }

      private def spawns(method: Symbol): Boolean = {
        val name = qualified(method)
        Spawning(name) || concurrent(name) || name.startsWith("scala.collection.parallel.")
      }

      /** Assignments in `body`, outside synchronized blocks, to variables declared outside it that are not volatile. */
      private def unguarded(body: tpd.Tree): Vector[(Symbol, tpd.Tree)] = {
        val declared = body.filterSubTrees {
          case tpd.ValDef(_, _, _) => true
          case _                   => false
        }.map(_.symbol).toSet
        val found = mutable.ArrayBuffer.empty[(Symbol, tpd.Tree)]
        Unguarded(declared, found).traverse(body)
        found.toVector
      }

      private class Unguarded(declared: Set[Symbol], found: mutable.ArrayBuffer[(Symbol, tpd.Tree)])
          extends tpd.TreeTraverser {
        override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
          case application: tpd.Apply if application.fun.symbol == defn.Object_synchronized => ()
          case assignment @ tpd.Assign(target, _)
              if target.symbol.is(Flags.Mutable) && !declared(target.symbol) &&
                !target.symbol.hasAnnotation(defn.VolatileAnnot) =>
            found += target.symbol -> assignment
            traverseChildren(tree)
          case _ => traverseChildren(tree)
        }
      }

      private def transformed(qualifier: Option[tpd.Tree]): Boolean = qualifier.exists { q =>
        val tpe = Calls.unconverted(q).tpe
        traversals.collection(tpe) || tpe.widen.derivesFrom(IteratorClass)
      }
    }
  }
}
