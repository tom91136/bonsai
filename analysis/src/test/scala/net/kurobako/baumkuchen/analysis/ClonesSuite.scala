package net.kurobako.baumkuchen.analysis

class ClonesSuite extends AnalysisSuite {
  private def findings(main: Seq[(String, String)], test: Seq[(String, String)] = Nil): Vector[Finding] = {
    val result = extract(main, test)
    Clones.findings(result.clones, result.graph.nodes)
  }

  private def average(owner: String, items: String, positive: String, doubled: String, method: String = "size") =
    s"""object $owner {
       |  def summary($items: List[Int], label: String): String = {
       |    val $positive = $items.filter(_ > 0)
       |    val $doubled = $positive.map(value => value * 2 + 1)
       |    val total = $doubled.foldLeft(0)(_ + _)
       |    val count = $doubled.$method
       |    if (count == 0) label else s"$$label: $${total / count}"
       |  }
       |}""".stripMargin

  test("bodies that differ only in local names are duplicates") {
    val found = findings(Seq(
      "First.scala"  -> average("First", "items", "kept", "scaled"),
      "Second.scala" -> average("Second", "values", "chosen", "twice")
    ))
    assertFinding(found, "duplicate-code", "2 copies of", "First.scala", "Second.scala")
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  test("dense expressions repeated across files are duplicates, even when short") {
    val found = findings(Seq(
      "Factors.scala" ->
        """object Factors {
                           |  def maximal(frequent: Vector[(Set[String], Set[Int])]): Vector[(Set[String], Set[Int])] =
                           |    frequent.groupMap(_._2)(_._1).toVector.flatMap { (cases, candidates) =>
                           |      candidates.filterNot(fields => candidates.exists(other => other.size > fields.size && fields.subsetOf(other)))
                           |        .map(_ -> cases)
                           |    }
                           |}""".stripMargin,
      "Clumps.scala" ->
        """object Clumps {
                          |  def clumps(frequent: Vector[(Set[String], Set[Int])]): Vector[(Set[String], Int)] =
                          |    frequent.groupMap(_._2)(_._1).toVector.flatMap { (methods, candidates) =>
                          |      candidates.filterNot(items => candidates.exists(other => other.size > items.size && items.subsetOf(other)))
                          |        .map(items => items -> methods.size)
                          |    }
                          |}""".stripMargin
    ))
    assertFinding(found, "duplicate-code", "2 copies of", "Factors.scala", "Clumps.scala")
  }

  test("calls to different methods are not duplicates") {
    assertEquals(
      findings(Seq(
        "First.scala"  -> average("First", "items", "kept", "scaled"),
        "Second.scala" -> average("Second", "items", "kept", "scaled", method = "length")
      )),
      Vector.empty
    )
  }

  test("inconsistent renamings are not duplicates") {
    val shared = (operation: String) =>
      s"""  def check(left: Int, right: Int): Int = {
         |    val scaled = left * 3 + right * 5
         |    val shifted = scaled - left + right
         |    val mixed = $operation + shifted * scaled
         |    mixed + scaled - shifted * 2
         |  }""".stripMargin
    assertEquals(
      findings(Seq(
        "First.scala"  -> s"object First {\n${shared("left * left")}\n}",
        "Second.scala" -> s"object Second {\n${shared("left * right")}\n}"
      )),
      Vector.empty
    )
  }

  test("runs of statements shared by otherwise different methods are duplicates") {
    val run =
      """    val positive = items.filter(_ > 0)
        |    val doubled = positive.map(value => value * 2 + 1)
        |    val total = doubled.foldLeft(0)(_ + _)
        |    val count = doubled.size""".stripMargin
    val found = findings(Seq(
      "First.scala" ->
        s"object First {\n  def first(items: List[Int]): Int = {\n    println(\"first\")\n$run\n    total - count\n  }\n}",
      "Second.scala" ->
        s"object Second {\n  def second(items: List[Int]): String = {\n$run\n    s\"$$total of $$count\"\n  }\n}"
    ))
    assertFinding(found, "duplicate-code", "2 copies of 4 lines")
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  test("generated sources are not hashed") {
    assertEquals(
      findings(Seq(
        "src/generated/First.scala"  -> average("First", "items", "kept", "scaled"),
        "src/generated/Second.scala" -> average("Second", "values", "chosen", "twice")
      )),
      Vector.empty
    )
  }

  test("duplicates only in tests are information") {
    val found = findings(
      Nil,
      Seq(
        "FirstSuite.scala"  -> average("First", "items", "kept", "scaled"),
        "SecondSuite.scala" -> average("Second", "values", "chosen", "twice")
      )
    )
    assertFinding(found, "duplicate-code", "(all in tests)")(using Finding.Level.Info)
  }

  test("null literals are hashed") {
    val nullable = (owner: String) =>
      s"""object $owner {
         |  def pick(items: List[String], fallback: String): String = {
         |    val first: String = items.headOption.getOrElse(null)
         |    val chosen = if (first == null) fallback else first
         |    val trimmed = chosen.trim.toLowerCase.replace(' ', '-')
         |    val counted = items.filter(_ != null).map(_.length).sum + trimmed.length * 2
         |    if (trimmed.isEmpty) null else trimmed + counted.toString + items.size.toString
         |  }
         |}""".stripMargin
    assertFinding(
      findings(Seq("First.scala" -> nullable("First"), "Second.scala" -> nullable("Second"))),
      "duplicate-code"
    )
  }

  test("the right-hand sides of pattern definitions are bodies too") {
    def split(owner: String) =
      s"""class $owner(items: List[Int]) {
         |  val (label, size) = {
         |    val positive = items.filter(_ > 0)
         |    val doubled  = positive.map(value => value * 2 + 1)
         |    val total    = doubled.foldLeft(0)(_ + _)
         |    val count    = doubled.size
         |    (if (count == 0) "none" else s"mean: $${total / count}", count)
         |  }
         |}""".stripMargin
    val found = findings(Seq("First.scala" -> split("First"), "Second.scala" -> split("Second")))
    assertFinding(found, "duplicate-code", "2 copies of", "First.scala", "Second.scala")
  }
}
