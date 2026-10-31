package net.kurobako.bonsai.analysis

/** The expressions the code leaves unused, from the sites an IDE reported as unused expressions in a Scala 3 build. */
class UnusedExpressionSuite extends AnalysisSuite {
  private val params =
    """case class Param(name: String, typ: Int, isConst: Boolean = false)
      |object Docs {
      |  def counts(params: List[Param]): Set[String] = params.collect {
      |    case Param(name = name) if params.exists(_.name == s"${name}Count") => name
      |  }.toSet
      |
      |  def arrays(fns: List[List[Param]]): Boolean = fns.exists(_.exists {
      |    case Param(typ = 1 | 2) => true
      |    case _                  => false
      |  })
      |
      |  def buffers(params: List[Param]): Boolean = params.exists {
      |    case Param(typ = 3 | 4) => true
      |    case _                  => false
      |  }
      |}""".stripMargin

  private def discards(sources: (String, String)*): Vector[String] = {
    val result = extract(sources)
    Report.findings(result).view.collect {
      case finding if finding.rule == Finding.Rule.PureDiscard => finding.detail.takeWhile(_ != ' ')
    }.toVector
  }

  test("a guard and the result of a case written with named patterns are used, whatever the case binds") {
    assertEquals(discards("Docs.scala" -> params), Vector.empty)
  }

  test("a pure call in statement position is unused, beside the cases that use their value") {
    val waste =
      """case class Param(name: String, typ: Int)
        |object Waste {
        |  def probe(params: List[Param]): Int = {
        |    params.exists(_.name.isEmpty)
        |    params.map(_.typ)
        |    params.size
        |  }
        |
        |  def used(params: List[Param]): Boolean = params.exists {
        |    case Param(typ = 1) => true
        |    case _              => false
        |  }
        |}""".stripMargin
    assertEquals(discards("Waste.scala" -> waste), Vector("List.exists", "List.map"))
  }
}
