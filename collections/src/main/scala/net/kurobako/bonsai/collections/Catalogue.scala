package net.kurobako.bonsai.collections

import java.util.regex.Pattern

/** The laws, in the order that breaks a tie between two of the same length. */
private[collections] object Catalogue {

  private def sequence(kind: Kind): Boolean = kind.library && kind.ordered && !kind.single && !kind.keyed &&
    !kind.distinct

  private def strictSequence(kind: Kind): Boolean = sequence(kind) && kind.strict

  private def strictRun(hit: Hit): Boolean = hit.steps.forall(step => step.receiver.strict && step.result.strict)

  private def unary(hit: Hit): Boolean = hit.steps.forall(_.args.size == 1)

  private def pure(hit: Hit): Boolean = hit.steps.forall(_.args.forall(_.pure))

  private val strictSequences =
    Set("immutable.List", "immutable.Vector", "immutable.ArraySeq", "immutable.Range", "immutable.Range.Inclusive")

  private val strictMaps = Set("immutable.Map", "immutable.HashMap", "immutable.TreeMap")

  private val linear = Set("immutable.List", "immutable.Seq", "Seq")

  private val sequences = strictSequences ++ Set("immutable.Seq", "Seq", "IndexedSeq", "immutable.IndexedSeq", "Array")

  private val primitiveKeyed = Set("immutable.LongMap", "immutable.IntMap")

  val identities: Map[String, Set[String]] = Map(
    "toSeq"        -> Set("immutable.Seq"),
    "toList"       -> Set("immutable.List"),
    "toVector"     -> Set("immutable.Vector"),
    "toIndexedSeq" -> Set("immutable.IndexedSeq"),
    "toMap"        -> strictMaps,
    "toSet"        -> Set("immutable.Set")
  )

  private def strictReceiver(hit: Hit): Boolean = strictSequences(hit.receiver.name)

  private def notString(hit: Hit): Boolean = hit.steps.forall(_.receiver.name != "String")

  private def int(arg: Arg): Option[Int] = arg.literal.collect { case n: Int => n }

  private def negation(negated: Boolean): String = if (negated) "!" else ""

  private val everyElement = "exists and forall stop at the first deciding one"
  private val sortedTails  = Set("head", "last", "headOption", "lastOption")

  private def oneMinMax(tail: String): String =
    if (tail.startsWith("last"))
      "max and maxBy compare in one pass, but return the first of equal elements where the sort ends with the last"
    else "min and minBy compare in one pass"

  private def projected(id: String, component: Int, method: String, name: String): Law = Law(
    id,
    Vector(Set("map")),
    into = method,
    idiom = true,
    applies = hit => hit.receiver.keyed && single(hit, 0).exists(_.projection.contains(component)),
    rewrite = Some((_, receiver) => s"$receiver.$method"),
    message = hit =>
      if (hit.steps.head.desugared) s"use $method for a for-comprehension that yields the $name component"
      else s"use $method for map of the $name component"
  )

  private def fresh(texts: Seq[String]): String = (Iterator("x", "y", "z") ++ Iterator.from(1).map(n => s"x$n")).find {
    name =>
      val word = Pattern.compile(s"(?<![\\w$$])${Pattern.quote(name)}(?![\\w$$])")
      !texts.exists(word.matcher(_).find())
  }.get

  private def renamed(id: String, pattern: Vector[Set[String]], into: String, at: Int, message: String): Law = Law(
    id,
    pattern,
    into = into,
    idiom = true,
    applies = _.args(at).sizeIs == 1,
    rewrite = Some((hit, receiver) => s"$receiver.$into(${hit.args(at).head.text})"),
    message = _ => message
  )

  private def fusion(method: String, body: (String, Vector[Arg]) => String): Law = {
    val id = method.replaceAll("([A-Z])", "-$1").toLowerCase
    Law(
      s"$id-$id-to-$id",
      Vector(Set(method)),
      into = method,
      repeat = true,
      deferred = true,
      analysed = true,
      applies = hit => strictRun(hit) && unary(hit),
      rewrite = Some { (hit, receiver) =>
        val functions = hit.steps.map(_.args.head)
        val name      = fresh(receiver +: functions.map(_.text))
        s"$receiver.$method($name => ${body(name, functions)})"
      },
      exact = pure(_),
      message = hit => s"use one $method for ${hit.names}: ${hit.gain}",
      report = Some { hit =>
        s"${hit.names} fuse into one $method, but the order of their effects would change unless both functions are " +
          s"pure: ${hit.gain}"
      }
    )
  }

  private def joined(operator: String)(name: String, functions: Vector[Arg]): String =
    functions.map(function => Source.operand(function.applied(name), operator)).mkString(s" $operator ")

  private def composed(name: String, functions: Vector[Arg]): String =
    functions.foldLeft(name)((value, function) => function.applied(Source.argument(value)))

  private val hashedSets = Set("immutable.Set", "immutable.HashSet", "mutable.Set", "mutable.HashSet", "Set")

  private val hashedMaps = Set("immutable.Map", "immutable.HashMap", "mutable.Map", "mutable.HashMap", "Map")

  private def single(hit: Hit, at: Int): Option[Arg] = Option.when(hit.args(at).sizeIs == 1)(hit.args(at).head)

  private def comparison(hit: Hit, at: Int, equal: Boolean): Option[Comparison] =
    single(hit, at).flatMap(_.comparison).filter(_.equal == equal)

  /** The constant of an equality test, or an inequality test, that `contains` takes on the receiver: any on a strict
    * sequence, one the elements conform to on a hashed set.
    */
  private def tested(hit: Hit, equal: Boolean): Option[String] =
    comparison(hit, 0, equal).filter(found =>
      strictSequences(hit.receiver.name) || hashedSets(hit.receiver.name) && found.assignable
    ).map(_.constant)

  private val containsMessage = "use contains for equality with a constant"

  private def constantLaw(
      id: String,
      method: String,
      render: (String, String) => String,
      found: Hit => Option[String]
  ): Law =
    Law(
      id,
      Vector(Set(method)),
      into = "contains",
      idiom = true,
      applies = hit => found(hit).isDefined,
      rewrite = Some((hit, receiver) => render(receiver, found(hit).get)),
      message = _ => containsMessage
    )

  private def constantTest(id: String, method: String, render: (String, String) => String): Law =
    constantLaw(id, method, render, hit => tested(hit, method != "forall"))

  private def constantFind(id: String, render: (String, String) => String): Law =
    constantLaw(id, "find", render, hit => tested(hit, equal = true).filter(_ => hashedSets(hit.receiver.name)))
      .copy(idiom = false)

  /** The keys of a hashed map searched for a constant, which `contains` of the map looks up. */
  private def keyTest(id: String, method: String, wrap: String => String): Law = {
    val equal              = method == "exists"
    def constant(hit: Hit) = comparison(hit, 1, equal).filter(_.assignable).map(_.constant)
    Law(
      id,
      Vector(Set("keys"), Set(method)),
      into = "contains",
      idiom = true,
      applies = hit => hit.receiver.keyed && hashedMaps(hit.receiver.name) && constant(hit).isDefined,
      rewrite = Some((hit, receiver) => wrap(s"$receiver.contains(${constant(hit).get})")),
      message = _ => containsMessage
    )
  }

  private def foldedInto(hit: Hit): String =
    if (hit.method(1) == "map") "the yield of the for-comprehension" else "a generator of the for-comprehension"

  private def nested(hit: Hit, receiver: String): String = {
    val first = hit.args(0).head
    val name  = fresh(receiver +: hit.steps.map(_.args.head.text))
    s"$receiver.flatMap($name => ${Source.argument(first.applied(name))}.${hit.method(1)}(${hit.args(1).head.text}))"
  }

  private def slice(id: String, first: String, second: String, bounds: (Int, Int) => Option[(Int, Int)]): Law = Law(
    id,
    Vector(Set(first), Set(second)),
    into = "slice",
    applies = hit => strictReceiver(hit) && bound(hit, bounds).isDefined,
    rewrite = Some { (hit, receiver) =>
      val (from, until) = bound(hit, bounds).get
      s"$receiver.slice($from, $until)"
    },
    message = _ => "use slice for constant nonnegative bounds"
  )

  private def bound(hit: Hit, bounds: (Int, Int) => Option[(Int, Int)]): Option[(Int, Int)] =
    for {
      a      <- single(hit, 0).flatMap(int(_))
      b      <- single(hit, 1).flatMap(int(_))
      result <- bounds(a, b)
    } yield result

  private def zero(hit: Hit): Boolean = strictReceiver(hit) && single(hit, 0).exists(_.literal.contains(0))

  private def emptiness(id: String, check: String, method: String): Law = {
    val negated = check == "sizeNonZero"
    val emptied = s"${negation(negated)}isEmpty()"
    Law(
      id,
      Vector(Set(check)),
      into = method,
      idiom = true,
      applies = notString(_),
      rewrite = Some((_, receiver) => s"$receiver.$method"),
      message = _ => s"use $method instead of a size comparison with zero",
      java = Some(JavaForm(
        _ => s"use $emptied instead of a size comparison with zero",
        Some((_, receiver) => s"${negation(negated)}$receiver.isEmpty()"),
        _.receiver.strict
      ))
    )
  }

  private def filteredEmptiness(id: String, check: String): Law = {
    val matcher = if (check == "sizeIsZero") "noneMatch" else "anyMatch"
    Law(
      id,
      Vector(Set("filter", "filterNot"), Set(check)),
      into = "exists",
      applies = hit => single(hit, 0).isDefined && notString(hit),
      rewrite = Some { (hit, receiver) =>
        val negated = hit.method(0) == "filterNot"
        s"${negation((check == "sizeIsZero") != negated)}$receiver.${quantifier(hit)}(${hit.args(0).head.text})"
      },
      message = hit => s"use ${quantifier(hit)} instead of ${hit.method(0)} and a size comparison with zero",
      java = Some(JavaForm(
        _ => s"use $matcher instead of filter and a count compared with zero",
        Some((hit, receiver) => s"$receiver.$matcher(${hit.args(0).head.text})"),
        hit => hit.method(0) == "filter" && pure(hit)
      ))
    )
  }

  private def firstMatch(id: String, method: String, into: String): Law = Law(
    id,
    Vector(Set(method), Set("headOption", "lastOption")),
    into = into,
    idiom = true,
    message = hit => s"$method then ${hit.method(1)} traverses every element; $into stops at the first match"
  )

  /** `contains` of the one argument, negated when step `at` is the method `negated`. */
  private def containsOf(at: Int, negated: String)(hit: Hit, receiver: String): String =
    s"${negation(hit.method(at) == negated)}$receiver.contains(${hit.args(0).head.text})"

  private def quantifier(hit: Hit): String = if (hit.method(0) == "filterNot") "forall" else "exists"

  private def lifted(hit: Hit): Option[String] =
    if (hit.method(0) == "liftLast") Some("last")
    else Option.when(single(hit, 0).exists(_.literal.contains(0)))("head")

  private def noneCheck(hit: Hit): String = if (hit.method(0) == "isSome") "isDefined" else "isEmpty"

  private def present(hit: Hit): Boolean = hit.method(0) == "isSome"

  private def presenceCall(present: Boolean): String = if (present) "isPresent()" else "isEmpty()"

  private def matchOf(hit: Hit): String = if (Set("isEmpty", "isNone")(hit.method(1))) "noneMatch" else "anyMatch"

  private def presence(hit: Hit): String = if (hit.method(1) == "isEmpty") "isEmpty" else "isPresent"

  private def reduction(hit: Hit): String =
    if (hit.method(0) == "joinReduceStrict")
      "use String.join instead of reducing a stream by concatenation, which returns an empty text where orElseThrow fails"
    else "use String.join instead of reducing a stream by concatenation"

  private val factories = Map("unmodifiableList" -> "List", "unmodifiableSet" -> "Set", "unmodifiableMap" -> "Map")

  private def copied(hit: Hit): String = s"${factories(hit.method(0))}.copyOf"

  private def separator(hit: Hit): String = hit.args(1).headOption.fold("\"\"")(_.text)

  /** A law of the Java code only, whose steps the Java front end lowers to and whose rewrite is `render`. */
  private def javaOnly(
      id: String,
      pattern: Vector[Set[String]],
      into: String,
      idiom: Boolean,
      message: Hit => String,
      render: (Hit, String) => String,
      applies: Hit => Boolean = _ => true
  ): Law = Law(
    id,
    pattern,
    into = into,
    idiom = idiom,
    applies = applies,
    message = message,
    java = Some(JavaForm(message, Some(render)))
  )

  /** A stream of a collection searched for the elements equal to a constant, or the ones that differ from it, which
    * `contains` of the collection looks up. A sorted collection compares by its comparator, so it is left out.
    */
  private def streamContains(id: String, matcher: String, wrap: String => String): Law = {
    val equal              = matcher != "forall"
    def constant(hit: Hit) = comparison(hit, 1, equal).map(_.constant)
    val method             = Set(matcher)
    val quantifier         = if (matcher == "exists") "anyMatch" else if (matcher == "forall") "allMatch" else matcher
    val sense              = if (equal) "an equality test" else "an inequality test"
    javaOnly(
      id,
      Vector(Set("stream"), method),
      "contains",
      idiom = true,
      _ => s"use ${wrap("contains")} instead of stream and $quantifier of $sense",
      (hit, receiver) => wrap(s"$receiver.contains(${constant(hit).get})"),
      hit => hit.receiver.name.startsWith("java.util.") && !hit.receiver.sorted && constant(hit).isDefined
    )
  }

  val laws: Vector[Law] = Vector(
    renamed(
      "filter-to-count",
      Vector(Set("filter"), Set("size", "length")),
      "count",
      0,
      "use count for filter and size"
    ),
    Law(
      "filter-emptiness",
      Vector(Set("filter", "filterNot"), Set("isEmpty", "nonEmpty")),
      into = "exists",
      idiom = true,
      message = hit => s"filtering then ${hit.method(1)} traverses every element; $everyElement"
    ),
    firstMatch("filter-head-option", "filter", "find"),
    firstMatch("collect-head-option", "collect", "collectFirst"),
    Law(
      "filter-map-to-collect",
      Vector(Set("filter", "filterNot"), Set("map")),
      into = "collect",
      idiom = true,
      analysed = true,
      applies = hit => !(hit.steps.head.desugared && single(hit, 1).exists(_.identity)),
      message = hit =>
        if (hit.steps.head.desugared)
          "a guard then yield is a collect; a partial function with a guard replaces the for-comprehension"
        else s"${hit.method(0)} then map is a collect; a partial function with a guard filters and maps in one pass"
    ),
    Law(
      "find-to-exists",
      Vector(Set("find"), Set("isDefined", "nonEmpty", "isEmpty", "isSome", "isNone")),
      into = "exists",
      applies = _.args(0).sizeIs == 1,
      rewrite = Some { (hit, receiver) =>
        val exists = s"$receiver.exists(${hit.args(0).head.text})"
        if (Set("isEmpty", "isNone")(hit.method(1))) s"!$exists" else exists
      },
      message = _ => "use exists for find and an emptiness check",
      java = Some(JavaForm(
        hit => s"use ${matchOf(hit)} for filter, findFirst and ${presence(hit)}",
        Some((hit, receiver) => s"$receiver.${matchOf(hit)}(${hit.args(0).head.text})"),
        hit => Set("isDefined", "isEmpty")(hit.method(1))
      ))
    ),
    renamed(
      "map-flatten-to-flat-map",
      Vector(Set("map"), Set("flatten")),
      "flatMap",
      0,
      "use flatMap for map and flatten"
    ),
    Law(
      "flat-map-identity-to-flatten",
      Vector(Set("flatMap")),
      into = "flatten",
      idiom = true,
      applies = hit => single(hit, 0).exists(_.identity),
      rewrite = Some((_, receiver) => s"$receiver.flatten"),
      message = hit =>
        if (hit.steps.head.desugared) "use flatten for generators that yield the elements of the elements"
        else "use flatten for flatMap of identity"
    ),
    renamed(
      "reverse-take-to-take-right",
      Vector(Set("reverse"), Set("take"), Set("reverse")),
      "takeRight",
      1,
      "use takeRight for reverse and take and reverse"
    ),
    Law(
      "reverse-find",
      Vector(Set("reverse"), Set("find")),
      into = "findLast",
      idiom = true,
      message = _ =>
        "reverse then find reverses the collection and searches from the front; findLast searches from the end"
    ),
    Law(
      "sorted-min-max",
      Vector(Set("sorted", "sortBy", "sortWith"), sortedTails),
      into = "min",
      applies = hit => (hit.method(0) == "sorted") == hit.args(0).isEmpty,
      message = hit => s"${hit.method(0)} then ${hit.method(1)} sorts every element; ${oneMinMax(hit.method(1))}",
      java = Some(JavaForm(
        _ => "sorted then findFirst sorts every element; min compares in one pass",
        Some { (hit, receiver) =>
          val order = hit.args(0).headOption.fold("Comparator.naturalOrder()")(_.text)
          s"$receiver.min($order)"
        },
        _.method(1) == "headOption"
      ))
    ),
    Law(
      "to-set-and-back-to-distinct",
      Vector(Set("toSet"), Set("toSeq", "toList")),
      into = "distinct",
      applies = hit => strictSequence(hit.receiver),
      rewrite = Some((_, receiver) => s"$receiver.distinct"),
      message = hit => s"use distinct for toSet then ${hit.method(1)}"
    ),
    Law(
      "unzip-to-map",
      Vector(Set("unzip", "unzip3"), Set("_1", "_2", "_3")),
      into = "map",
      idiom = true,
      applies = hit => hit.method(0) == "unzip3" || hit.method(1) != "_3",
      rewrite = Some((hit, receiver) => s"$receiver.map(_.${hit.method(1)})"),
      message = hit => s"use map(_.${hit.method(1)}) for unzip and a component"
    ),
    Law(
      "zip-indices-to-zip-with-index",
      Vector(Set("zip")),
      into = "zipWithIndex",
      idiom = true,
      applies = hit =>
        hit.from == 0 && strictSequence(hit.receiver) && hit.args(0).sizeIs == 1 &&
          hit.args(0).head.text == s"${hit.chain.source.text}.indices",
      rewrite = Some((_, receiver) => s"$receiver.zipWithIndex"),
      message = _ => "use zipWithIndex for zip of the indices"
    ),
    Law(
      "range-to-indices",
      Vector(Set("untilSize", "toLastIndex")),
      into = "indices",
      idiom = true,
      applies = hit => strictSequence(hit.receiver),
      rewrite = Some((_, receiver) => s"$receiver.indices"),
      message = _ => "use indices for a range over the size"
    ),
    projected("map-to-keys", 1, "keys", "first"),
    projected("map-to-values", 2, "values", "second"),
    Law(
      "map-contains",
      Vector(Set("map"), Set("contains")),
      into = "exists",
      idiom = true,
      applies = hit => single(hit, 1).exists(_.literal.exists(_.isInstanceOf[Boolean])),
      message = _ => s"map then contains traverses every element; $everyElement"
    ),
    Law(
      "fold-true-and-to-forall",
      Vector(Set("foldTrueAnd")),
      into = "forall",
      applies = _.args(0).sizeIs == 1,
      rewrite = Some((hit, receiver) => s"$receiver.forall(${hit.args(0).head.text})"),
      message = _ => "use forall for a fold that ands a predicate"
    ),
    constantTest("exists-to-contains", "exists", (receiver, constant) => s"$receiver.contains($constant)"),
    constantTest("forall-to-not-contains", "forall", (receiver, constant) => s"!($receiver.contains($constant))"),
    constantFind(
      "exists-to-contains",
      (receiver, constant) => s"Option.when($receiver.contains($constant))($constant)"
    ),
    keyTest("exists-to-contains", "exists", identity),
    keyTest("forall-to-not-contains", "forall", text => s"!($text)"),
    constantTest("exists-to-contains", "countNonZero", (receiver, constant) => s"$receiver.contains($constant)"),
    constantTest("forall-to-not-contains", "countZero", (receiver, constant) => s"!$receiver.contains($constant)"),
    Law(
      "filter-contains-to-intersect",
      Vector(Set("filter")),
      into = "intersect",
      idiom = true,
      applies =
        hit => hit.receiver.name == "immutable.Set" && hit.args(0).sizeIs == 1 && hit.args(0).head.membership.isDefined,
      rewrite = Some((hit, receiver) => s"$receiver.intersect(${hit.args(0).head.membership.get})"),
      message = _ => "use intersect instead of filter and contains"
    ),
    Law(
      "get-get-or-else",
      Vector(Set("get"), Set("getOrElse")),
      into = "getOrElse",
      applies = hit => strictMaps(hit.receiver.name) && hit.args(0).sizeIs == 1 && hit.args(1).sizeIs == 1,
      rewrite = Some((hit, receiver) => s"$receiver.getOrElse(${hit.args(0).head.text}, ${hit.args(1).head.text})"),
      message = _ => "use getOrElse(key, default)"
    ),
    slice(
      "drop-take-to-slice",
      "drop",
      "take",
      (from, length) =>
        Option.when(from >= 0 && length >= 0 && from.toLong + length <= Int.MaxValue)((from, from + length))
    ),
    slice(
      "take-drop-to-slice",
      "take",
      "drop",
      (length, from) => Option.when(from >= 0 && length >= 0)((from, length))
    ),
    Law(
      "drop-zero",
      Vector(Set("drop")),
      into = "self",
      applies = zero(_),
      rewrite = Some((_, receiver) => receiver),
      message = _ => "drop(0) returns the collection itself"
    ),
    Law(
      "take-zero",
      Vector(Set("take")),
      into = "empty",
      applies = zero(_),
      message = _ => "take(0) is always empty; return an empty collection or check the bound"
    ),
    Law(
      "size-to-length",
      Vector(Set("size")),
      into = "length",
      idiom = true,
      applies = hit => Set("Array", "String")(hit.receiver.name),
      rewrite = Some((_, receiver) => s"$receiver.length"),
      message = _ => "use length on an array or string"
    ),
    emptiness("size-to-is-empty", "sizeIsZero", "isEmpty"),
    emptiness("size-to-non-empty", "sizeNonZero", "nonEmpty"),
    filteredEmptiness("size-to-is-empty", "sizeIsZero"),
    filteredEmptiness("size-to-non-empty", "sizeNonZero"),
    Law(
      "filter-before-sorted",
      Vector(Set("sorted"), Set("filter")),
      into = "sorted",
      applies = _.args(1).sizeIs == 1,
      message = _ => "prefer filter(...).sorted when changing predicate and ordering evaluation is safe"
    ),
    Law(
      "redundant-collection-conversion",
      Vector(identities.keySet),
      into = "self",
      applies = hit =>
        identities(hit.method(0))(hit.receiver.name) ||
          (hit.method(0) == "toSeq" && strictReceiver(hit) && hit.chain.asserted && hit.until == hit.chain.steps.size),
      rewrite = Some((_, receiver) => receiver),
      message = _ => "remove the identity collection conversion"
    ),
    Law(
      "map-get-to-apply",
      Vector(Set("get"), Set("get")),
      into = "apply",
      applies = hit => hit.receiver.keyed && hit.args(0).sizeIs == 1 && hit.args(1).isEmpty,
      rewrite = Some((hit, receiver) => s"$receiver(${hit.args(0).head.text})"),
      message = _ => "use the map as a function instead of get and get"
    ),
    Law(
      "map-get-emptiness",
      Vector(Set("get"), Set("isEmpty", "nonEmpty", "isDefined")),
      into = "contains",
      applies = hit => hit.receiver.keyed && !primitiveKeyed(hit.receiver.name) && hit.args(0).sizeIs == 1,
      rewrite = Some(containsOf(1, "isEmpty")),
      message = _ => "use contains instead of get and an emptiness check"
    ),
    Law(
      "redundant-option-iteration",
      Vector(Set("headOption", "lastOption")),
      into = "self",
      idiom = true,
      applies = _.receiver.single,
      rewrite = Some((_, receiver) => receiver),
      message = hit => s"Option.${hit.method(0)} is the option itself"
    ),
    Law(
      "map-get-or-else-to-fold",
      Vector(Set("map"), Set("getOrElse")),
      into = "fold",
      idiom = true,
      applies = hit =>
        hit.receiver.single && single(hit, 0).exists(!_.identity) && single(hit, 1).isDefined,
      message = _ => "use fold for map and getOrElse"
    ),
    Law(
      "redundant-identity-map",
      Vector(Set("map")),
      into = "self",
      idiom = true,
      applies = hit => hit.receiver.single && single(hit, 0).exists(_.identity),
      rewrite = Some((_, receiver) => receiver),
      message = _ => "map of identity is the option itself"
    ),
    Law(
      "map-lift-to-get",
      Vector(Set("lift")),
      into = "get",
      idiom = true,
      applies = hit => hit.receiver.keyed && hit.args(0).sizeIs == 1,
      rewrite = Some((hit, receiver) => s"$receiver.get(${hit.args(0).head.text})"),
      message = _ => "use get for lift on a map"
    ),
    Law(
      "head-or-last-option",
      Vector(Set("lift", "liftLast")),
      into = "lastOption",
      idiom = true,
      applies = hit => sequences(hit.receiver.name) && lifted(hit).isDefined,
      rewrite = Some((hit, receiver) => s"$receiver.${lifted(hit).get}Option"),
      message = hit => {
        val method = lifted(hit).get
        s"use ${method}Option for lift of the ${if (method == "head") "first" else "last"} index"
      }
    ),
    Law(
      "zero-index-to-head",
      Vector(Set("apply")),
      into = "head",
      idiom = true,
      applies = hit => linear(hit.receiver.name) && single(hit, 0).exists(_.literal.contains(0)),
      rewrite = Some((_, receiver) => s"$receiver.head"),
      message = _ => "use head for the first element"
    ),
    Law(
      "last-index-to-last",
      Vector(Set("applyLast")),
      into = "last",
      idiom = true,
      applies = hit => linear(hit.receiver.name),
      rewrite = Some((_, receiver) => s"$receiver.last"),
      message = _ => "use last for the last element"
    ),
    Law(
      "some-to-contains",
      Vector(Set("equalsSome", "notEqualsSome")),
      into = "contains",
      idiom = true,
      applies = hit => hit.receiver.single && hit.args(0).sizeIs == 1,
      rewrite = Some(containsOf(0, "notEqualsSome")),
      message = _ => "use contains for an equality check with Some"
    ),
    Law(
      "none-check",
      Vector(Set("isNone", "isSome")),
      into = "isEmpty",
      idiom = true,
      applies = _.receiver.single,
      rewrite = Some((hit, receiver) => s"$receiver.${noneCheck(hit)}"),
      message = hit => s"use ${noneCheck(hit)} for a check against None",
      java = Some(JavaForm(
        hit => s"use ${presenceCall(present(hit))} instead of !${presenceCall(!present(hit))}",
        Some((hit, receiver) => s"$receiver.${presenceCall(present(hit))}")
      ))
    ),
    Law(
      "flat-map-nesting",
      Vector(Set("flatMap"), Set("map", "filter", "filterNot", "flatMap", "collect")),
      into = "flatMap",
      deferred = true,
      analysed = true,
      applies = hit =>
        strictRun(hit) && unary(hit) && hit.receiver.name != "Array" &&
          (!hit.steps.head.desugared || Set("map", "flatMap")(hit.method(1))),
      rewrite = Some(nested(_, _)),
      exact = hit => pure(hit) && !hit.args(1).head.text.startsWith("case"),
      message = hit =>
        if (hit.steps.head.desugared) s"fold the ${hit.method(1)} into ${foldedInto(hit)}: ${hit.gain}"
        else s"use one flatMap for ${hit.names}: ${hit.gain}",
      report = Some { hit =>
        if (hit.steps.head.desugared)
          s"the ${hit.method(1)} after a for-comprehension belongs in ${foldedInto(hit)}, but the order of effects " +
            s"would change unless both functions are pure: ${hit.gain}"
        else
          s"${hit.names} nest into one flatMap, but the order of their effects would change unless both functions " +
            s"are pure: ${hit.gain}"
      }
    ),
    javaOnly(
      "stream-of",
      Vector(Set("of", "asList"), Set("stream")),
      "streamOf",
      idiom = false,
      hit => s"use Stream.of instead of ${hit.chain.source.text}.${hit.method(0)}(...).stream()",
      (hit, _) => s"Stream.of(${hit.args(0).map(_.text).mkString(", ")})"
    ),
    javaOnly(
      "stream-join",
      Vector(Set("stream"), Set("join")),
      "stringJoin",
      idiom = true,
      _ => "use String.join instead of collecting a stream with joining",
      (hit, receiver) => s"String.join(${separator(hit)}, $receiver)"
    ),
    streamContains("exists-to-contains", "exists", identity),
    streamContains("forall-to-not-contains", "forall", text => s"!$text"),
    streamContains("forall-to-not-contains", "noneMatch", text => s"!$text"),
    Law(
      "stream-join",
      Vector(Set("joinReduce", "joinReduceStrict")),
      into = "join",
      idiom = true,
      message = reduction(_),
      java = Some(JavaForm(reduction(_)))
    ),
    javaOnly(
      "java9-collection-factory",
      Vector(factories.keySet),
      "copyOf",
      idiom = false,
      hit => s"use ${copied(hit)} instead of ${hit.method(0)} of a copy",
      (hit, receiver) => s"${copied(hit)}($receiver)"
    ),
    fusion("map", composed(_, _)),
    fusion("filter", joined("&&")),
    fusion("filterNot", joined("||")),
    Law(
      "intermediate-collections",
      Vector(Ops.elementWise),
      into = "fused",
      repeat = true,
      advice = true,
      deferred = true,
      viewable = true,
      applies = strictRun(_),
      message = hit => s"${hit.names}: use a view to fuse them, ${hit.gain}"
    )
  )
}
