package net.kurobako.bonsai.analysis

class RepeatedCodeSuite extends AnalysisSuite {
  private def findings(main: Seq[(String, String)], test: Seq[(String, String)] = Nil): Vector[Finding] = {
    val result = extract(main, test, generated = GeneratedSources)
    Clones.findings(result.clones, result.graph.nodes)
  }

  private def reported(found: Vector[Finding], rule: String): Vector[Finding] = found.filter(_.rule.id == rule)

  private def shortened(sites: Int) = (1 to sites).map(i =>
    s"  def label$i(key: String): String = key.split('.').last + \"$i\""
  ).mkString(
    "object Designs {\n",
    "\n",
    "\n}"
  )

  test("a call chain written at many sites is a repeated expression, even though each copy is small") {
    val found = findings(Seq("Shortened.scala" -> shortened(17)))
    assertFinding(found, "duplicate-code", "17 copies of 1 line", "Shortened.scala")
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  test("a constructor call repeated within one method is a repeated expression") {
    val found = findings(Seq(
      "Effects.scala" ->
        """case class Summary(effects: Set[String], runs: Boolean)
          |object Effects {
          |  def of(kind: String, runs: Boolean): Summary = kind match {
          |    case "pure"    => Summary(Set.empty, runs && kind.nonEmpty)
          |    case "console" => Summary(Set("io"), runs && kind.nonEmpty)
          |    case "read"    => Summary(Set.empty, runs && kind.nonEmpty)
          |    case "write"   => Summary(Set("mutation"), runs && kind.nonEmpty)
          |    case "math"    => Summary(Set.empty, runs && kind.nonEmpty)
          |    case "string"  => Summary(Set.empty, runs && kind.nonEmpty)
          |    case "option"  => Summary(Set.empty, runs && kind.nonEmpty)
          |    case "list"    => Summary(Set.empty, runs && kind.nonEmpty)
          |    case _         => Summary(Set("unknown"), runs && kind.nonEmpty)
          |  }
          |}""".stripMargin
    ))
    assertFinding(found, "duplicate-code", "6 copies of 1 line", "Effects.scala")
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  test("a record built from one source by the same projections four times in a method is a repeated expression") {
    val found = findings(Seq(
      "Collector.scala" ->
        """case class Shape(kind: String, name: String, cases: Int, primitive: Boolean)
          |case class Element(name: String)
          |case class Parameter(name: String, kind: String, label: String, cases: Int, primitive: Boolean)
          |object Collector {
          |  def all(fields: List[Element], methods: List[Element], variants: List[Element], nested: List[Element], shape: Shape) =
          |    (
          |      fields.map(x => Parameter(x.name.toUpperCase, shape.kind, shape.name, shape.cases, shape.primitive)),
          |      methods.map(m => Parameter(m.name.toUpperCase, shape.kind, shape.name, shape.cases, shape.primitive)),
          |      variants.map(v => Parameter(v.name.toUpperCase, shape.kind, shape.name, shape.cases, shape.primitive)),
          |      nested.map(n => Parameter(n.name.toUpperCase, shape.kind, shape.name, shape.cases, shape.primitive))
          |    )
          |}""".stripMargin
    ))
    assertFinding(found, "duplicate-code", "4 copies of", "Collector.scala")
  }

  test("calls with a single argument and chains too short to matter are not repeated expressions") {
    val sites = (1 to 9).map(i =>
      s"  def names$i(items: List[Element]): List[String] = items.map(_.name) ++ List(\"$i\")"
    )
    val found = findings(Seq(
      "Elements.scala" -> sites.mkString("case class Element(name: String)\nobject Elements {\n", "\n", "\n}")
    ))
    assertEquals(found, Vector.empty)
  }

  test("an expression repeated too few times for its size, in different methods, is not a repeated expression") {
    assertEquals(findings(Seq("Shortened.scala" -> shortened(14))), Vector.empty)
  }

  test("an expression repeated fewer than four times in one method is not a repeated expression") {
    val found = findings(Seq(
      "Effects.scala" ->
        """case class Summary(effects: Set[String], runs: Boolean)
          |object Effects {
          |  def of(kind: String, runs: Boolean): Summary = kind match {
          |    case "pure"    => Summary(Set.empty, runs)
          |    case "console" => Summary(Set("io"), runs)
          |    case "read"    => Summary(Set.empty, runs)
          |    case "write"   => Summary(Set("mutation"), runs)
          |    case "math"    => Summary(Set.empty, runs)
          |    case _         => Summary(Set("unknown"), runs)
          |  }
          |}""".stripMargin
    ))
    assertEquals(found, Vector.empty)
  }

  test("calls that differ in a literal are not repeated expressions") {
    val parts = Seq('.', ',', ';', ':', '/', '-').zipWithIndex.map { (separator, i) =>
      s"  def part$i(key: String): String = key.split('$separator').last"
    }
    assertEquals(findings(Seq("Parts.scala" -> parts.mkString("object Parts {\n", "\n", "\n}"))), Vector.empty)
  }

  test("the arguments the compiler passes for a using clause make no call chain") {
    val sites = (1 to 40).map(i =>
      s"  def check$i(symbol: Symbol)(using Context): Boolean = symbol.owner.isClass && $i > 0"
    )
    val found = findings(Seq(
      "Symbols.scala" ->
        ("""class Context
          |class Symbol {
          |  def owner(using Context): Symbol = this
          |  def isClass(using Context): Boolean = true
          |}
          |""".stripMargin + sites.mkString("object Symbols {\n", "\n", "\n}"))
    ))
    assertEquals(found, Vector.empty)
  }

  test("calls that leave their arguments to defaults are not repeated expressions") {
    val branches = (1 to 8).map(i =>
      s"""    case "--flag$i" :: rest => parse(rest, options.copy(nesting = options.nesting.copy(depth = $i)))"""
    )
    val found = findings(Seq(
      "Cli.scala" ->
        (s"""case class Config(depth: Int = 2, damping: Int = 1, size: Int = 200, enabled: Boolean = false)
            |case class Options(first: Int = 0, second: Int = 0, third: Int = 0, fourth: Int = 0, nesting: Config = Config())
            |object Cli {
            |  def parse(args: List[String], options: Options): Options = args match {
            |${branches.mkString("\n")}
            |    case _ => options
            |  }
            |}""".stripMargin)
    ))
    assertEquals(reported(found, "duplicate-code"), Vector.empty)
  }

  test("values that hold state are not constants to share") {
    val found = findings(
      Seq("First", "Second", "Third", "Fourth").map(name =>
        s"$name.scala" ->
          s"""import scala.collection.mutable
             |object $name {
             |  private val looping = mutable.Stack(false)
             |  def run(): Boolean = looping.push(true).nonEmpty
             |}""".stripMargin
      )
    )
    assertEquals(found, Vector.empty)
  }

  test("the rows of a table written in a value are not repeated expressions") {
    val rows = (1 to 12).map(i =>
      s"""    Rule("rule$i", hit => hit.args(0).sizeIs == 1 && hit.args(1).sizeIs == $i),"""
    )
    val found = findings(Seq(
      "Rules.scala" ->
        s"""case class Hit(args: List[List[Int]])
           |case class Rule(name: String, applies: Hit => Boolean)
           |object Rules {
           |  val all = List(
           |${rows.mkString("\n")}
           |    Rule("last", hit => hit.args.isEmpty)
           |  )
           |}""".stripMargin
    ))
    assertEquals(found, Vector.empty)
  }

  test("an expression repeated only in tests is left to the tests") {
    assertEquals(
      findings(Seq("Main.scala" -> "object Main"), test = Seq("ShortenedSuite.scala" -> shortened(17))),
      Vector.empty
    )
  }

  test("private values with equal small bodies in several objects are duplicates") {
    val found = findings(Seq(
      "Clones.scala" ->
        """object Clones {
          |  private val Width = "[0-9]+".r
          |  def widths(text: String): List[String] = Width.findAllIn(text).toList
          |}""".stripMargin,
      "Designs.scala" ->
        """object Designs {
          |  private val Width = "[0-9]+".r
          |  def width(text: String): Option[String] = Width.findFirstIn(text)
          |}""".stripMargin,
      "Extraction.scala" ->
        """object Extraction {
          |  private val Width = "[0-9]+".r
          |  def widthless(text: String): String = Width.replaceAllIn(text, "")
          |}""".stripMargin
    ))
    assertFinding(found, "duplicate-code", "3 copies of 1 line", "Clones.scala", "Designs.scala", "Extraction.scala")
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  test("private methods with equal small bodies in different objects are duplicates") {
    val found = findings(Seq(
      "Designs.scala" ->
        """object Designs {
          |  private def short(name: String): String = name.split('.').last
          |  def labels(names: List[String]): List[String] = names.map(short(_)).sorted
          |}""".stripMargin,
      "Structures.scala" ->
        """object Structures {
          |  private def short(key: String): String = key.split('.').last
          |  def labelled(keys: List[String]): Set[String] = keys.map(short(_)).toSet
          |}""".stripMargin
    ))
    assertFinding(found, "duplicate-code", "2 copies of 1 line", "Designs.scala", "Structures.scala")
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  test("small bodies that are mere accessors, or differ in a literal, are not duplicate helpers") {
    val found = findings(Seq(
      "Shared.scala"    -> "object Shared { val items: List[Int] = List(1, 2, 3) }",
      "Accessors.scala" ->
        """object First { def size: Int = Shared.items.size }
          |object Second { def size: Int = Shared.items.size }""".stripMargin,
      "Patterns.scala" ->
        """object Digits { private val Pattern = "[0-9]+".r; def of(text: String) = Pattern.findFirstIn(text) }
          |object Letters { private val Pattern = "[a-z]+".r; def of(text: String) = Pattern.findFirstIn(text) }""".stripMargin
    ))
    assertEquals(found, Vector.empty)
  }

  test("small bodies of members that implement an abstract member are not duplicate helpers") {
    val found = findings(Seq(
      "Named.scala" ->
        """trait Named { def label: String }
          |object First extends Named { override def label: String = getClass.getName.stripSuffix("$").toLowerCase }
          |object Second extends Named { override def label: String = getClass.getName.stripSuffix("$").toLowerCase }""".stripMargin
    ))
    assertEquals(found, Vector.empty)
  }

  private val Selected =
    """case class Config(
      |    disabled: Option[Set[String]],
      |    errors: Option[Set[String]],
      |    warnings: Option[Set[String]],
      |    silenced: Option[Set[String]]
      |)
      |object Defaults { val names: List[String] = List("all") }
      |object Plugin {
      |  def own(configs: Seq[Config], id: String): Option[Config] = configs.find(_.disabled.exists(_.contains(id)))
      |  def disabled(configs: Seq[Config], id: String, extras: Set[String]): Set[String] =
      |    own(configs, id).flatMap(_.disabled).map(_ ++ extras).getOrElse(Defaults.names.toSet)
      |  def errors(configs: Seq[Config], id: String, extras: Set[String]): Set[String] =
      |    own(configs, id).flatMap(_.errors).map(_ ++ extras).getOrElse(Defaults.names.toSet)
      |  def warnings(configs: Seq[Config], id: String, extras: Set[String]): Set[String] =
      |    own(configs, id).flatMap(_.warnings).map(_ ++ extras).getOrElse(Defaults.names.toSet)
      |  def silenced(configs: Seq[Config], id: String, extras: Set[String]): Set[String] =
      |    own(configs, id).flatMap(_.silenced).map(_ ++ extras).getOrElse(Defaults.names.toSet)
      |}""".stripMargin

  test("copies that differ only in a selected field are near duplicates") {
    val found = findings(Seq("Plugin.scala" -> Selected))
    assertFinding(found, "near-duplicate-code", "4 copies of", "{_.disabled, _.errors, _.warnings, _.silenced}")
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  test("copies that differ in a constant they refer to and a literal are near duplicates") {
    def phased(name: String, phase: String, limit: Int) =
      s"""  def $name(input: List[String]): List[Int] =
         |    Reports.send(Phase.$phase, input.map(_.trim).filter(_.nonEmpty).map(_.toLowerCase).map(_.length + $limit)).sorted""".stripMargin
    val found = findings(Seq(
      "Reports.scala" ->
        """enum Phase { case Parse, Lower, Emit }
          |object Reports { def send(phase: Phase, sizes: List[Int]): List[Int] = sizes.map(_ + phase.ordinal) }""".stripMargin,
      "Phases.scala" ->
        Seq(phased("parse", "Parse", 1), phased("lower", "Lower", 2), phased("emit", "Emit", 3))
          .mkString("object Phases {\n", "\n", "\n}")
    ))
    assertFinding(found, "near-duplicate-code", "3 copies of", "Phase.Emit")
  }

  test("copies that differ in the side of an either are near duplicates") {
    def sided(name: String, side: String, other: String) =
      s"""  def $name(values: List[Int], limit: Int): Either[List[Int], List[Int]] = {
         |    val kept = values.filter(_ > limit).map(_ * 2).map(_ + 1).sorted
         |    if (kept.size > limit) $side(kept.take(limit).distinct) else $other(kept.reverse.distinct)
         |  }""".stripMargin
    val found = findings(Seq(
      "Sides.scala" ->
        Seq(sided("high", "Left", "Right"), sided("low", "Right", "Left"), sided("mid", "Left", "Left"))
          .mkString("object Sides {\n", "\n", "\n}")
    ))
    assertFinding(found, "near-duplicate-code", "3 copies of")
  }

  test("alike blocks in two methods are near duplicates") {
    def scaled(name: String, limit: Int) =
      s"""  def $name(values: List[Int]): List[Int] = {
         |    val kept = values.filter(_ > $limit).map(_ * 2).map(_ + 1).sorted
         |    kept.take($limit).distinct.reverse
         |  }""".stripMargin
    val found = findings(Seq(
      "Scales.scala" -> Seq(scaled("small", 1), scaled("large", 3)).mkString("object Scales {\n", "\n", "\n}")
    ))
    assertFinding(found, "near-duplicate-code", "2 copies of", "{1, 3}")
  }

  test("alike statements in one block passed to a call are near duplicates") {
    def sided(side: String, other: String) =
      s"""    println(if (values.filter(_ > limit).map(_ * 2).sorted.size > limit)
         |      $side(values.filter(_ > limit).map(_ * 2).sorted.take(limit).distinct)
         |    else $other(values.filter(_ > limit).map(_ * 2).sorted.reverse.distinct))""".stripMargin
    val found = findings(Seq(
      "Sides.scala" ->
        s"""object Sides {
           |  def run(body: => Unit): Unit = body
           |  def all(values: List[Int], limit: Int): Unit = run {
           |${Seq(sided("Left", "Right"), sided("Right", "Left"), sided("Left", "Left")).mkString("\n")}
           |  }
           |}""".stripMargin
    ))
    assertFinding(found, "near-duplicate-code", "3 copies of")
  }

  test("alike laws that are elements of one collection literal are rows, not near duplicates") {
    val found = findings(Seq(
      "Laws.scala" ->
        """case class Hit(args: List[List[Int]])
          |case class Law(
          |  id: String,
          |  pattern: Vector[Set[String]],
          |  into: String,
          |  message: Hit => String,
          |  idiom: Boolean = false
          |)
          |object Laws {
          |  val all: Vector[Law] = Vector(
          |    Law(
          |      "filter-emptiness",
          |      Vector(Set("filter", "filterNot"), Set("isEmpty", "nonEmpty"), Set("exists")),
          |      into = "exists",
          |      idiom = true,
          |      message = hit => s"filtering then ${hit.args(1)} traverses every element"
          |    ),
          |    Law(
          |      "reverse-find",
          |      Vector(Set("filter", "filterNot"), Set("isEmpty", "nonEmpty"), Set("exists")),
          |      into = "findLast",
          |      idiom = true,
          |      message = hit => s"filtering then ${hit.args(2)} traverses every element"
          |    )
          |  )
          |}""".stripMargin
    ))
    assertEquals(found, Vector.empty)
  }

  test("calls of one function that differ only in a reference they pass are not near duplicates") {
    val found = findings(Seq(
      "Reports.scala" ->
        """enum Phase { case Parse, Lower, Emit }
          |object Reports { def send(phase: Phase, sizes: List[Int], names: List[String]): List[Int] = sizes }
          |object Phases {
          |  def parse(input: List[String]): List[Int] = Reports.send(Phase.Parse, input.map(_.length), input.map(_.trim))
          |  def lower(input: List[String]): List[Int] = Reports.send(Phase.Lower, input.map(_.length), input.map(_.trim))
          |  def emit(input: List[String]): List[Int] = Reports.send(Phase.Emit, input.map(_.length), input.map(_.trim))
          |}""".stripMargin
    ))
    assertEquals(found, Vector.empty)
  }

  test("copies that differ in three places are not near duplicates") {
    val found = findings(Seq(
      "Config.scala" ->
        """case class Config(disabled: Option[Set[String]], errors: Option[Set[String]], warnings: Option[Set[String]])
          |object Defaults { val names: List[String] = List("all"); val kinds: List[String] = List("some"); val extras: List[String] = Nil }
          |object Limits { val low: Int = 1; val mid: Int = 2; val high: Int = 3 }
          |object Plugin {
          |  def disabled(c: Config, id: String): Set[String] =
          |    c.disabled.map(_ ++ Defaults.names).filter(_.size > Limits.low).getOrElse(Defaults.names.toSet).filter(_ != id)
          |  def errors(c: Config, id: String): Set[String] =
          |    c.errors.map(_ ++ Defaults.kinds).filter(_.size > Limits.mid).getOrElse(Defaults.kinds.toSet).filter(_ != id)
          |  def warnings(c: Config, id: String): Set[String] =
          |    c.warnings.map(_ ++ Defaults.extras).filter(_.size > Limits.high).getOrElse(Defaults.extras.toSet).filter(_ != id)
          |}""".stripMargin
    ))
    assertEquals(found, Vector.empty)
  }

  test("three copies of moderate size that differ in a selected field are not near duplicates") {
    val three = Selected.split("  def silenced").head + "}"
    assertEquals(findings(Seq("Plugin.scala" -> three)), Vector.empty)
  }

  test("two copies that differ in a selected field, or copies across files, are not near duplicates") {
    val two = Selected.split("  def warnings").head + "}"
    assertEquals(findings(Seq("Plugin.scala" -> two)), Vector.empty)
    def apart(owner: String, field: String) =
      s"""object $owner {
         |  def pick(configs: Seq[Config], id: String, extras: Set[String]): Set[String] =
         |    Plugin.own(configs, id).flatMap(_.$field).map(_ ++ extras).getOrElse(Defaults.names.toSet)
         |}""".stripMargin
    val spread = findings(Seq(
      "Plugin.scala" -> two,
      "A.scala"      -> apart("A", "disabled"),
      "B.scala"      -> apart("B", "errors"),
      "C.scala"      -> apart("C", "warnings"),
      "D.scala"      -> apart("D", "silenced")
    ))
    assertEquals(reported(spread, "near-duplicate-code"), Vector.empty)
  }

  test("small copies that differ in a reference are not near duplicates") {
    val sites = Seq("disabled", "errors", "warnings").map(field =>
      s"  def $field(c: Config): Int = c.$field.map(_.size).getOrElse(0)"
    )
    val found = findings(Seq(
      "Small.scala" ->
        sites.mkString(
          "case class Config(disabled: Option[Set[String]], errors: Option[Set[String]], warnings: Option[Set[String]])\nobject Small {\n",
          "\n",
          "\n}"
        )
    ))
    assertEquals(found, Vector.empty)
  }

  private val Elements =
    """class Trees { def path(unit: Int, leaf: String): String = leaf; def element(path: String): String = path }
      |object Context {
      |  def elementOf(trees: Trees, unit: Int, leaf: String): Option[String] =
      |    Option(trees.path(unit, leaf)).flatMap(path => Option(trees.element(path)))
      |}""".stripMargin

  test("an expression that is the body of a helper, applied to other values, is an inlined helper") {
    val found = findings(Seq(
      "Context.scala" -> Elements,
      "Lints.scala"   ->
        """object Lints {
          |  def run(trees: Trees, unit: Int, tree: String): Option[String] =
          |    Option(trees.path(unit, tree)).flatMap(path => Option(trees.element(path))).map(_.trim)
          |}""".stripMargin
    ))
    assertFinding(found, "inlined-helper", "Lints.scala", "elementOf")
    assertEquals(reported(found, "inlined-helper").size, 1, found.mkString("\n"))
  }

  test("a helper applied to a field and a literal is still an inlined helper") {
    val found = findings(Seq(
      "Context.scala" -> Elements,
      "Lints.scala"   ->
        """object Lints {
          |  val trees = new Trees
          |  val current: String = "leaf"
          |  def run(): Option[String] =
          |    Option(trees.path(3, current)).flatMap(path => Option(trees.element(path))).map(_.trim)
          |}""".stripMargin
    ))
    assertFinding(found, "inlined-helper", "Lints.scala", "elementOf")
  }

  test("a call that leaves its arguments to their defaults is no helper") {
    val found = findings(Seq(
      "Costs.scala" ->
        """case class Cost(passes: Int = 0, allocations: Int = 0, early: Int = 0)
          |object Costs {
          |  private val constant = Cost()
          |  def sum(left: Cost, right: Cost): Cost = Cost(left.passes + right.passes)
          |  def none: List[Cost] = List(Cost())
          |}""".stripMargin
    ))
    assertEquals(reported(found, "inlined-helper"), Vector.empty)
  }

  test("a private helper is reported at the copies inside its object, a private helper elsewhere is no helper") {
    def helper(access: String) =
      s"""object Names {
         |  $access def moduleOf(nodes: Map[String, (String, Int)], name: String): Option[String] =
         |    nodes.get(name).map(_._1).filter(_.nonEmpty).map(_.toLowerCase)
         |  def inside(nodes: Map[String, (String, Int)], key: String): Option[String] =
         |    nodes.get(key).map(_._1).filter(_.nonEmpty).map(_.toLowerCase).orElse(None)
         |}""".stripMargin
    val outside =
      """object Elsewhere {
        |  def lookup(nodes: Map[String, (String, Int)], key: String): Option[String] =
        |    nodes.get(key).map(_._1).filter(_.nonEmpty).map(_.toLowerCase).orElse(Some("x"))
        |}""".stripMargin
    val found = findings(Seq("Names.scala" -> helper("private"), "Elsewhere.scala" -> outside))
    assertFinding(found, "inlined-helper", "Names.scala", "moduleOf")
    assertEquals(reported(found, "inlined-helper").size, 1, found.mkString("\n"))
    val open = findings(Seq("Names.scala" -> helper("  "), "Elsewhere.scala" -> outside))
    assertEquals(reported(open, "inlined-helper").size, 2, open.mkString("\n"))
  }

  test(
    "the body of a helper, a recursive call of it, and a copy of its body with other literals are no inlined helper"
  ) {
    val found = findings(Seq(
      "Context.scala" -> Elements,
      "Lints.scala"   ->
        """object Lints {
          |  def run(trees: Trees, unit: Int, tree: String): Option[String] =
          |    Option(trees.path(unit + 1, tree)).flatMap(path => Option(trees.element(path))).map(_.trim)
          |}""".stripMargin
    ))
    assertEquals(reported(found, "inlined-helper"), Vector.empty)
  }

  test("a helper that holds its parameter twice is no helper for a copy that holds two values") {
    val found = findings(Seq(
      "Context.scala" ->
        """object Context {
          |  def spread(low: List[Int]): List[Int] = low.map(_ * 2).filter(_ > 3).take(low.size).distinct.sorted
          |}""".stripMargin,
      "Lints.scala" ->
        """object Lints {
          |  def run(low: List[Int], high: List[Int]): List[Int] =
          |    low.map(_ * 2).filter(_ > 3).take(high.size).distinct.sorted.reverse
          |}""".stripMargin
    ))
    assertEquals(reported(found, "inlined-helper"), Vector.empty)
  }

  test("an instance method of a class is no helper for code outside the class") {
    val found = findings(Seq(
      "Context.scala" ->
        """class Context(trees: List[String]) {
          |  def elementOf(leaf: String): Option[String] = trees.find(_.startsWith(leaf)).map(_.trim).filter(_.nonEmpty)
          |}""".stripMargin,
      "Lints.scala" ->
        """object Lints {
          |  def run(trees: List[String], tree: String): Option[String] =
          |    trees.find(_.startsWith(tree)).map(_.trim).filter(_.nonEmpty).orElse(None)
          |}""".stripMargin
    ))
    assertEquals(reported(found, "inlined-helper"), Vector.empty)
  }

  private val Codecs =
    """trait Codec[A] { def imap[B](to: A => B, from: B => A): Codec[B] }
      |object Codec { def of[A](name: String, size: Int): Codec[A] = null }""".stripMargin

  private val Table = "Map[String, List[Int]]"
  private val Build = s"""Codec.of[$Table]("json".trim, "x".length + 1)"""

  private val CodecSite =
    s"""object Models {
       |  val lengths = $Build.imap[Int](_.size, _ => Map.empty)
       |}""".stripMargin

  private def codecs(json: String) = Seq("Codecs.scala" -> Codecs, "Json.scala" -> json, "Models.scala" -> CodecSite)

  test("an anonymous given is no helper") {
    val found = findings(codecs(s"object Json { given Codec[$Table] = $Build }"))
    assertEquals(reported(found, "inlined-helper"), Vector.empty, found.mkString("\n"))
  }

  test("a given over an opaque type, which the call would give another type than the copy, is no helper") {
    val found = findings(codecs(
      s"""object Json {
         |  opaque type Json = $Table
         |  given codec: Codec[Json] = $Build
         |}""".stripMargin
    ))
    assertEquals(reported(found, "inlined-helper"), Vector.empty, found.mkString("\n"))
  }

  test("a method whose result is an opaque type that its body is not is no helper") {
    val found = findings(codecs(
      s"""object Json {
         |  opaque type Json = $Table
         |  def codec(): Codec[Json] = $Build
         |}""".stripMargin
    ))
    assertEquals(reported(found, "inlined-helper"), Vector.empty, found.mkString("\n"))
  }

  test("a method whose declared result is wider than its body is no helper") {
    val found = findings(codecs(s"object Json { def codec(): Any = $Build }"))
    assertEquals(reported(found, "inlined-helper"), Vector.empty, found.mkString("\n"))
  }

  test("a method whose declared result is its body type is a helper") {
    val found = findings(codecs(s"object Json { def codec(): Codec[$Table] = $Build }"))
    assertFinding(found, "inlined-helper", "Models.scala", "codec")
  }

  private def documented(parameter: String, site: String) =
    Seq(
      "Emitter.scala" ->
        s"""trait Described { val name: String; val description: Option[String] }
           |case class SubItem(name: String, description: Option[String]) extends Described
           |case class Domain(name: String, description: Option[String])
           |case class Special(name: String, description: Option[String]) extends Described
           |object Emitter {
           |  def docOf(item: $parameter): String =
           |    item.description.map(_.trim).filter(_.nonEmpty).map(_.toLowerCase).getOrElse(item.name)
           |}""".stripMargin,
      "Main.scala" ->
        s"""object Main {
           |  def run(value: $site): String =
           |    value.description.map(_.trim).filter(_.nonEmpty).map(_.toLowerCase).getOrElse(value.name).trim
           |}""".stripMargin
    )

  test("a helper is no helper for a value that its parameter does not accept") {
    val found = findings(documented("SubItem", "Domain"))
    assertEquals(reported(found, "inlined-helper"), Vector.empty, found.mkString("\n"))
  }

  test("a helper is no helper for a value of a supertype of its parameter") {
    val found = findings(documented("SubItem", "Described"))
    assertEquals(reported(found, "inlined-helper"), Vector.empty, found.mkString("\n"))
  }

  test("a helper is a helper for a value of a subtype of its parameter, and of its own type") {
    assertFinding(findings(documented("Described", "Special")), "inlined-helper", "Main.scala", "docOf")
    assertFinding(findings(documented("SubItem", "SubItem")), "inlined-helper", "Main.scala", "docOf")
  }

  test("a helper over a type parameter is a helper for a value of any type") {
    val found = findings(Seq(
      "Emitter.scala" ->
        """object Emitter {
          |  def docOf[A](item: Option[A], fallback: A): String =
          |    item.map(_.toString.trim).filter(_.nonEmpty).map(_.toLowerCase).getOrElse(fallback.toString)
          |}""".stripMargin,
      "Main.scala" ->
        """object Main {
          |  def run(value: Option[Int], other: Int): String =
          |    value.map(_.toString.trim).filter(_.nonEmpty).map(_.toLowerCase).getOrElse(other.toString).trim
          |}""".stripMargin
    ))
    assertFinding(found, "inlined-helper", "Main.scala", "docOf")
  }

  private val BaseChain =
    """name.trim.toLowerCase.stripPrefix("cef_").stripSuffix("_t")
      |      .replace("-", "_").replace(" ", "_").filter(_ != '.').take(64).mkString""".stripMargin

  private def baseNames(helper: String) =
    s"""object Naming {
       |  def baseName(name: String): String = $helper
       |}
       |object Writer {
       |  def header(name: String): String = {
       |    val base = $BaseChain
       |    base + "_N.cpp"
       |  }
       |  def handler(name: String): String = {
       |    val base = $BaseChain
       |    base + ".cpp"
       |  }
       |  def folder(name: String): String = {
       |    val base = $BaseChain
       |    base.toUpperCase
       |  }
       |}""".stripMargin

  test("copies of the body of a helper are reported as inlined helpers, not also as a duplicate group") {
    val found = findings(Seq("Naming.scala" -> baseNames(BaseChain)))
    assertEquals(reported(found, "inlined-helper").size, 3, found.mkString("\n"))
    assertEquals(reported(found, "duplicate-code"), Vector.empty, found.mkString("\n"))
  }

  test("copies of an expression that no helper holds are a duplicate group") {
    val found = findings(Seq("Naming.scala" -> baseNames("name")))
    assertFinding(found, "duplicate-code", "3 copies")
    assertEquals(reported(found, "inlined-helper"), Vector.empty, found.mkString("\n"))
  }

  test("a method too small to be worth calling is no helper") {
    val found = findings(Seq(
      "Context.scala" -> "object Context { def named(leaf: String): Boolean = leaf.nonEmpty }",
      "Lints.scala"   -> "object Lints { def run(tree: String): Boolean = tree.nonEmpty && tree.length > 3 }"
    ))
    assertEquals(found, Vector.empty)
  }

  test("a method that implements an abstract member is no helper") {
    val found = findings(Seq(
      "Context.scala" ->
        """trait Resolver { def elementOf(leaf: String): Option[String] }
          |object Context extends Resolver {
          |  override def elementOf(leaf: String): Option[String] = Option(leaf).map(_.trim).filter(_.nonEmpty).map(_.toLowerCase)
          |}""".stripMargin,
      "Lints.scala" ->
        """object Lints {
          |  def run(tree: String): Option[String] = Option(tree).map(_.trim).filter(_.nonEmpty).map(_.toLowerCase).orElse(None)
          |}""".stripMargin
    ))
    assertEquals(reported(found, "inlined-helper"), Vector.empty)
  }

  private def kernel(member: String) =
    s"""import scala.collection.mutable
       |import scala.reflect.ClassTag
       |object Vec { def newBuilder[A: ClassTag](hint: Int): mutable.Builder[A, Array[A]] = Array.newBuilder[A] }
       |class Kernel {
       |  $member
       |  def lower(rows: Int): Array[String] = {
       |    val columns = Vec.newBuilder[String](4)
       |    (0 until rows).foreach(row => columns += row.toString)
       |    columns.result()
       |  }
       |}""".stripMargin

  test("a value that makes a new stateful object is no helper for code that makes another") {
    val found = findings(Seq("Kernel.scala" -> kernel("private val before = Vec.newBuilder[String](4)")))
    assertEquals(reported(found, "inlined-helper"), Vector.empty, found.mkString("\n"))
  }

  test("a method that makes a new stateful object is a helper for code that makes another") {
    val found = findings(Seq("Kernel.scala" -> kernel("private def fresh() = Vec.newBuilder[String](4)")))
    assertFinding(found, "inlined-helper", "Kernel.scala", "fresh")
  }

  private def loader(statement: String) = {
    val cases = ('a' to 'm').map(kind => s"""      case ("$kind", dest, value) => $statement""")
    s"""import scala.collection.mutable
       |object Loader {
       |  private def id(value: Int): Int = value + 1
       |  def load(rows: List[(String, Int, Int)], constants: mutable.Map[Int, Int], names: StringBuilder): Unit =
       |    rows.foreach {
       |${cases.mkString("\n")}
       |      case _ => ()
       |    }
       |}""".stripMargin
  }

  test("a call that returns nothing, repeated in a method, is no expression to bind") {
    val found = findings(Seq("Loader.scala" -> loader("constants.update(id(dest), value)")))
    assertEquals(found, Vector.empty)
  }

  test("a chain of calls that returns a value, repeated in a method, is an expression to bind") {
    val found = findings(Seq("Loader.scala" -> loader("names.append(id(dest)).append(value)")))
    assertFinding(found, "duplicate-code", "13 copies of 1 line", "Loader.scala")
  }

  private val Reals =
    """class Real { def toFloat: Float = 1f }
      |class Half extends Real
      |class Single extends Real""".stripMargin

  test("overloads that take different types repeat a small body by design") {
    val found = findings(Seq(
      "Reals.scala"  -> Reals,
      "Floats.scala" ->
        """object Floats {
          |  def lt(left: Half, right: Half): Boolean = left.toFloat < right.toFloat && left.toFloat >= 0f
          |  def lt(left: Single, right: Single): Boolean = left.toFloat < right.toFloat && left.toFloat >= 0f
          |}""".stripMargin
    ))
    assertEquals(found, Vector.empty)
  }

  test("overloads that take different types and repeat a large body are duplicates") {
    def summary(kind: String) =
      s"""  def summary(left: $kind, right: $kind): String = {
         |    val low = List(left.toFloat, right.toFloat).filter(_ > 0f)
         |    val scaled = low.map(value => value * 2f + 1f)
         |    val total = scaled.foldLeft(0f)(_ + _)
         |    if (scaled.isEmpty) "none" else s"$${scaled.size}: $${total / scaled.size}"
         |  }""".stripMargin
    val found = findings(Seq(
      "Reals.scala"   -> Reals,
      "Reports.scala" -> s"object Reports {\n${summary("Half")}\n${summary("Single")}\n}"
    ))
    assertFinding(found, "duplicate-code", "2 copies of", "Reports.scala")
  }

  test("literals converted to a refined type are no copies to share") {
    def lanes(name: String) =
      name + ".scala" ->
        s"""import scala.language.implicitConversions
           |import Refined.given
           |object $name { val Lanes: Refined.Positive = 256 }""".stripMargin
    val found = findings(Seq(
      "Refined.scala" ->
        """import scala.language.implicitConversions
          |object Refined {
          |  opaque type Positive = Int
          |  given Conversion[Int, Positive] = value => value
          |}""".stripMargin,
      lanes("First"),
      lanes("Second"),
      lanes("Third")
    ))
    assertEquals(found, Vector.empty)
  }

  test("a call that passes only references and literals is no expression to bind") {
    val cases = Seq("toString", "abs", "toLong", "toDouble", "toHexString", "sign", "toChar", "toBinaryString")
    val found = findings(Seq(
      "Stages.scala" ->
        s"""object Family { val Core = "core" }
           |object Stages {
           |  def staged(site: Int, kind: Int, family: String): Int = site + kind + family.length
           |  def run(op: String, site: Int, kind: Int): Any = op match {
           |${cases.zipWithIndex.map((name, index) =>
            s"""    case "$index" => staged(site, kind, Family.Core).$name"""
          ).mkString("\n")}
           |    case _ => ()
           |  }
           |}""".stripMargin
    ))
    assertEquals(found, Vector.empty)
  }

  test("methods that only call another method with their parameters are no duplicate helpers") {
    val found = findings(Seq(
      "Faults.scala" ->
        "class Fault(detail: String); object Fault { def backend(detail: String): Fault = new Fault(detail) }",
      "First.scala"  -> "object First { private def invalid(detail: String): Fault = Fault.backend(detail) }",
      "Second.scala" -> "object Second { private def invalid(detail: String): Fault = Fault.backend(detail) }"
    ))
    assertEquals(found, Vector.empty)
  }
}
