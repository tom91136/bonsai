package net.kurobako.baumkuchen.analysis

class ReportSuite extends AnalysisSuite {
  private lazy val result = extract(Seq(
    "Main.scala" ->
      "object Main { def main(args: Array[String]): Unit = { println(Tools.used); println(Quiet.Inner().toString) } }",
    "Tools.scala" ->
      """object Tools {
                       |  def used = 1
                       |  def spare = 2
                       |  def other = 3
                       |  @SuppressWarnings(Array("baumkuchen:unused-member")) def kept = 4
                       |  private def helper = 5
                       |}
                       |@SuppressWarnings(Array("baumkuchen")) object Quiet { def spare = 6; class Inner { def spare = 7 } }
                       |class Dead""".stripMargin
  ))

  private def lines(options: Report.Options): Vector[Report.Line] =
    Report(Report.findings(result), result.graph.nodes, options)

  private val defaults = Report.Options(Set.empty, Set.empty, Set.empty, info = false)
  private val every    = Finding.Rule.values.map(_.id).toSet

  private def details(found: Vector[Report.Line], level: String): Vector[String] =
    found.filter(_.level == level).map(_.finding.detail)

  test("suppression applies to a member by rule, and to a type with everything declared inside it") {
    val found = lines(defaults).map(line => s"${line.finding.node.name}: ${line.finding.detail}")
    assert(found.contains("Tools: Tools.spare is never referenced"), found.mkString("\n"))
    assert(found.contains("Dead: "), found.mkString("\n"))
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

  test("@unused and @nowarn suppress like @SuppressWarnings, by rule when nowarn filters on a baumkuchen id") {
    val annotated = extract(Seq(
      "Main.scala"  -> "object Main { def main(args: Array[String]): Unit = println(Tools.used) }",
      "Tools.scala" ->
        """import scala.annotation.nowarn
          |import scala.annotation.unused
          |object Tools {
          |  def used = 1
          |  @unused def kept = 2
          |  @nowarn def quiet = 3
          |  @nowarn("msg=baumkuchen:unused-member") def filtered = 4
          |  @nowarn("msg=baumkuchen:weaker-access") def other = 5
          |  @nowarn("cat=deprecation") def deprecated = 6
          |}
          |@unused class Spare""".stripMargin
    ))
    val found = Report(Report.findings(annotated), annotated.graph.nodes, defaults).map(_.finding.detail)
    assertEquals(found.sorted, Vector("Tools.deprecated is never referenced", "Tools.other is never referenced"))
  }

  test("baselines round trip through their text form") {
    val found = lines(defaults)
    assertEquals(Report.baseline(Report.write(found)), found.map(_.finding.key).toSet)
  }

  test("escalating every rule fails on any warning, while information stays information") {
    val found = lines(defaults.copy(failing = every, info = true))
    assert(found.exists(_.failing), found.mkString("\n"))
    assert(found.filter(_.finding.level == Finding.Level.Warning).forall(_.failing), found.mkString("\n"))
    assert(found.filter(_.finding.level == Finding.Level.Info).forall(!_.failing), found.mkString("\n"))
  }
}
