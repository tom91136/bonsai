package net.kurobako.bonsai.analysis

class ReportSuite extends AnalysisSuite {
  private lazy val result = extract(Seq(
    "Main.scala" ->
      "object Main { def main(args: Array[String]): Unit = { println(Tools.used); println(Quiet.Inner().toString) } }",
    "Tools.scala" ->
      """object Tools {
                       |  def used = 1
                       |  def spare = 2
                       |  def other = 3
                       |  @SuppressWarnings(Array("bonsai:unused-member")) def kept = 4
                       |  private def helper = 5
                       |}
                       |@SuppressWarnings(Array("bonsai")) object Quiet { def spare = 6; class Inner { def spare = 7 } }
                       |class Dead""".stripMargin
  ))

  private def lines(options: Report.Options): Vector[Report.Line] =
    Report(Report.findings(result), result.graph.nodes, options)

  private val defaults = Report.Options(Set.empty, Set.empty, Set.empty, info = false)
  private val every    = Finding.Rule.values.map(_.id).toSet

  private def details(found: Vector[Report.Line], level: String): Vector[String] =
    found.view.collect { case line if line.level == level => line.finding.detail }.toVector

  test("suppression applies to a member by rule, and to a type with everything declared inside it") {
    val found = lines(defaults).map(line => s"${line.finding.node.name}: ${line.finding.detail}")
    assert(found.contains("Tools: Tools.spare is never referenced"), found.mkString("\n"))
    assert(found.contains("Dead: Dead is unreachable from every entry point"), found.mkString("\n"))
    assert(!found.exists(detail => detail.contains("kept") || detail.contains("Quiet") || detail.contains("Inner")))
  }

  test("escalated rules fail unless the finding is in the baseline, and disabled rules are dropped") {
    val spare = lines(defaults).map(_.finding).find(_.detail == "Tools.spare is never referenced").get
    val found = lines(defaults.copy(
      disabled = Set("unused"),
      failing = Set("unused-member", "weaker-access"),
      baseline = Set(spare.key),
      info = true
    ))
    assertEquals(details(found, "error"), Vector("Tools.other is never referenced"))
    assert(details(found, "warning").contains("Tools.spare is never referenced"), found.mkString("\n"))
    assert(!found.exists(_.finding.rule == Finding.Rule.Unused), found.mkString("\n"))
    assert(!found.exists(line => line.finding.level == Finding.Level.Info && line.level == "error"))
  }

  test("findings naming long types stay on one line") {
    val wide = extract(Seq(
      "Shape.scala" ->
        """type Wide = Either[Map[String, List[Option[Vector[Int]]]], Map[String, List[Option[Vector[Long]]]]] & Serializable
          |sealed trait Shape
          |case class Circle(first: Wide, second: Wide) extends Shape
          |case class Square(first: Wide, second: Wide) extends Shape
          |object Main { def main(args: Array[String]): Unit = println(List[Shape](Circle(null, null), Square(null, null))) }
          |""".stripMargin
    ))
    val found = Report(Report.findings(wide), wide.graph.nodes, defaults.copy(info = true))
    assert(found.exists(_.finding.rule == Finding.Rule.IdenticalCases), found.mkString("\n"))
    assert(found.forall(line => !line.show.contains('\n') && !line.finding.key.contains('\n')), found.mkString("\n"))
  }

  test("@unused and @nowarn suppress like @SuppressWarnings, by rule when nowarn filters on a bonsai id") {
    val annotated = extract(Seq(
      "Main.scala"  -> "object Main { def main(args: Array[String]): Unit = println(Tools.used) }",
      "Tools.scala" ->
        """import scala.annotation.nowarn
          |import scala.annotation.unused
          |object Tools {
          |  def used = 1
          |  @unused def kept = 2
          |  @nowarn def quiet = 3
          |  @nowarn("msg=bonsai:unused-member") def filtered = 4
          |  @nowarn("msg=bonsai:weaker-access") def other = 5
          |  @nowarn("cat=deprecation") def deprecated = 6
          |}
          |@unused class Spare""".stripMargin
    ))
    val found = Report(Report.findings(annotated), annotated.graph.nodes, defaults).map(_.finding.detail)
    assertEquals(found.sorted, Vector("Tools.deprecated is never referenced", "Tools.other is never referenced"))
  }

  test("lines render as tab-separated fields with the category of their node") {
    val node = Node(
      "lib.Tools.spare",
      "core",
      Output.Scope.Main,
      "src/main/scala/Tools.scala",
      12,
      generated = false,
      exported = false,
      outer = None
    )
    val finding = Finding(Finding.Rule.UnusedMember, node, "Tools.spare is never referenced")
    assertEquals(
      Report.Line("error", finding).show,
      "error\tunused-member\thandwritten\tcore\tlib.Tools.spare\tsrc/main/scala/Tools.scala:12\t" +
        s"Tools.spare is never referenced\t${finding.id}"
    )
    assertEquals(Report.Line("error", finding).show.split('\t').last, finding.id)
    assert(Report.Line("error", finding).failing)
    assert(!Report.Line("warning", finding).failing)
    assertEquals(Report.Line("info", finding.copy(node = node.copy(generated = true))).show.split('\t')(2), "generated")
  }

  test("a finding keeps its id across moved lines and changed counts, and loses it with another rule, symbol or file") {
    val node = Node(
      "lib.Tools",
      "core",
      Output.Scope.Main,
      "src/Tools.scala",
      12,
      generated = false,
      exported = false,
      outer = None
    )
    val finding = Finding(Finding.Rule.ConstantArgument, node, "Tools.run(width).width is always 80 across 3 calls")
    assertEquals(finding.id.length, 10)
    assert(finding.id.forall(character => "0123456789abcdef".contains(character)), finding.id)
    assertEquals(finding.copy(node = node.copy(line = 40)).id, finding.id)
    assertEquals(finding.copy(detail = "Tools.run(width).width is always 90 across 4 calls").id, finding.id)
    assertNotEquals(finding.copy(rule = Finding.Rule.FlagArgument).id, finding.id)
    assertNotEquals(finding.copy(node = node.copy(name = "lib.Other")).id, finding.id)
    assertNotEquals(finding.copy(node = node.copy(source = "src/Other.scala")).id, finding.id)
    assertNotEquals(finding.copy(detail = "Tools.run(width).height is always 80 across 3 calls").id, finding.id)
  }

  test("findings that share an id are numbered apart, and hiding some does not renumber the others") {
    val node = Node(
      "lib.Tools",
      "core",
      Output.Scope.Main,
      "src/Tools.scala",
      12,
      generated = false,
      exported = false,
      outer = None
    )
    val first = Finding(Finding.Rule.DuplicateCode, node, "2 copies of 5 lines: A.scala:10-14, B.scala:3-7")
    val other = first.copy(node = node.copy(line = 30), detail = "2 copies of 5 lines: A.scala:30-34, B.scala:23-27")
    val note  = first.copy(node = node.copy(line = 50), level = Finding.Level.Info)
    val all   = Vector(first, other, note)
    def ids(options: Report.Options) = Report(all, Map(node.name -> node), options).map(_.id)
    assertEquals(ids(defaults.copy(info = true)), Vector(first.id, s"${first.id}-2", s"${first.id}-3"))
    assertEquals(ids(defaults), Vector(first.id, s"${first.id}-2"))
    assertEquals(ids(defaults.copy(disabled = Set("duplicate-code"))), Vector.empty)
    assertEquals(first.id, other.id)
  }

  test("a baseline keeps a finding whose lines moved or whose counts changed, and not one with other words") {
    val node = Node(
      "lib.Tools",
      "core",
      Output.Scope.Main,
      "src/Tools.scala",
      12,
      generated = false,
      exported = false,
      outer = None
    )
    val recorded = Finding(Finding.Rule.DuplicateCode, node, "2 copies of 5 lines (84 nodes): A.scala:10-14, B.scala:3-7")
    val moved    = recorded.copy(detail = "2 copies of 5 lines (68 nodes): A.scala:30-34, B.scala:23-27")
    val other    = recorded.copy(detail = "2 copies of 5 lines (68 nodes): A.scala:30-34, C.scala:23-27")
    val options  = defaults.copy(failing = Set("duplicate-code"), baseline = Set(recorded.key))
    def levels(finding: Finding) = Report(Vector(finding), Map(node.name -> node), options).map(_.level)
    assertEquals(levels(moved), Vector("warning"))
    assertEquals(levels(other), Vector("error"))
  }

  test("baselines round trip through their text form") {
    val found = lines(defaults)
    assertEquals(Report.keys(Report.write(found)), found.map(_.finding.key).toSet)
  }

  test("escalating every rule fails on any warning, while information stays information") {
    val found = lines(defaults.copy(failing = every, info = true))
    assert(found.exists(_.failing), found.mkString("\n"))
    assert(found.filter(_.finding.level == Finding.Level.Warning).forall(_.failing), found.mkString("\n"))
    assert(found.filter(_.finding.level == Finding.Level.Info).forall(!_.failing), found.mkString("\n"))
  }

  private lazy val methodical = extract(
    Seq(
      "Main.scala"   -> "object Main { def main(args: Array[String]): Unit = println(Costly.quiet(Nil)) }",
      "Costly.scala" ->
        """object Costly {
          |  def cubic(xs: List[Int]): Int = xs.map(x => xs.map(y => xs.count(_ == x + y))).size
          |  @SuppressWarnings(Array("bonsai:complexity")) def quiet(xs: List[Int]): Int =
          |    xs.map(x => xs.map(y => xs.count(_ == x + y))).size
          |  def copies(xs: List[Int], ys: List[Int], zs: List[Int]): Int = {
          |    val sets = xs.map(x => ys.toSet)
          |    xs.map(x => ys.map(y => zs.count(_ == y))).size + sets.size
          |  }
          |  @SuppressWarnings(Array("bonsai:space-complexity")) def copiesQuiet(
          |      xs: List[Int], ys: List[Int], zs: List[Int]): Int = {
          |    val sets = xs.map(x => ys.toSet)
          |    xs.map(x => ys.map(y => zs.count(_ == y))).size + sets.size
          |  }
          |  def plain(values: List[Int]): Int = values.map(_ + 1).filter(_ > 0).sum
          |  @SuppressWarnings(Array("bonsai:intermediate-collections")) def piped(values: List[Int]): Int =
          |    values.map(_ + 1).filter(_ > 0).sum
          |  def noisy(values: List[Int]): List[Int] = values.map { value => println(value); value }
          |  @SuppressWarnings(Array("bonsai:effectful-transform")) def printed(values: List[Int]): List[Int] =
          |    values.map { value => println(value); value }
          |}
          |trait Shape { def area: Int }
          |class Circle extends Shape { def area = 1 }
          |class Square extends Shape { def area = 2 }
          |class Triangle extends Shape { def area = 3 }
          |object Looping {
          |  def looped(shapes: Array[Shape]): Int = {
          |    var total = 0
          |    var index = 0
          |    while (index < shapes.length) {
          |      total += shapes(index).area
          |      index += 1
          |    }
          |    total
          |  }
          |  @SuppressWarnings(Array("bonsai:megamorphic-call")) def quiet(shapes: Array[Shape]): Int = {
          |    var total = 0
          |    var index = 0
          |    while (index < shapes.length) {
          |      total += shapes(index).area
          |      index += 1
          |    }
          |    total
          |  }
          |}""".stripMargin
    ),
    hot = Vector("Looping")
  )

  test("a suppression on a method silences the findings its rules locate at the enclosing type") {
    val found = Report(Report.findings(methodical), methodical.graph.nodes, defaults.copy(info = true))
      .map(_.finding.detail)
    def reported(prefix: String) = found.exists(_.startsWith(prefix))
    assert(reported("Costly.cubic is O"), found.mkString("\n"))
    assert(reported("Costly.copiesQuiet is O"), found.mkString("\n"))
    assert(reported("Costly.copies allocates"), found.mkString("\n"))
    assert(reported("Costly.plain: map then filter"), found.mkString("\n"))
    assert(reported("Looping.looped calls"), found.mkString("\n"))
    assert(!reported("Costly.quiet is O"), found.mkString("\n"))
    assert(!reported("Costly.copiesQuiet allocates"), found.mkString("\n"))
    assert(!reported("Costly.piped: map then filter"), found.mkString("\n"))
    assert(!reported("Looping.quiet calls"), found.mkString("\n"))
    assertEquals(found.count(_.startsWith("the function passed to map")), 1, found.mkString("\n"))
  }
}
