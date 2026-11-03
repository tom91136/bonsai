package net.kurobako.bonsai.analysis

class ComplexityRetentionSuite extends AnalysisSuite {
  private lazy val found = {
    val result = extract(
      program(
        "Retention",
        """object Retention {
          |  def command(header: String, defines: List[String]): List[String] =
          |    List("cpp") ++ defines.map(define => s"-D$define") ++ List(header)
          |  def spawn(header: String, defines: List[String]): Int =
          |    new ProcessBuilder(command(header, defines)*).start().waitFor()
          |  def passed(headers: List[String], defines: List[String]): List[Int] =
          |    headers.map { header =>
          |      val arguments = List("cpp") ++ defines.map(define => s"-D$define") ++ List(header)
          |      new ProcessBuilder(arguments*).start().waitFor()
          |    }
          |  def delegated(headers: List[String], defines: List[String]): List[Int] =
          |    headers.map(header => spawn(header, defines))
          |  def flattened(headers: List[String], defines: List[String]): List[String] =
          |    headers.flatMap(header => List("cpp") ++ defines.map(define => s"-D$define") ++ List(header))
          |  def appended(headers: List[String], defines: List[String]): List[String] = {
          |    val all = scala.collection.mutable.ListBuffer.empty[String]
          |    headers.foreach { header =>
          |      val arguments = List("cpp") ++ defines.map(define => s"-D$define") ++ List(header)
          |      all ++= arguments
          |    }
          |    all.toList
          |  }
          |  def returned(headers: List[String], defines: List[String]): List[List[String]] =
          |    headers.map(header => List("cpp") ++ defines.map(define => s"-D$define") ++ List(header))
          |  def delegatedReturn(headers: List[String], defines: List[String]): List[List[String]] =
          |    headers.map(header => command(header, defines))
          |  val shared = scala.collection.mutable.ListBuffer.empty[String]
          |  def stored(headers: List[String], defines: List[String]): Unit =
          |    headers.foreach(header => shared ++= List("cpp") ++ defines.map(define => s"-D$define") ++ List(header))
          |  def keep(into: scala.collection.mutable.ListBuffer[String], arguments: List[String]): Unit = into ++= arguments
          |  def release(arguments: List[String]): Int = arguments.size
          |  def kept(headers: List[String], defines: List[String]): Unit =
          |    headers.foreach(header => keep(shared, List("cpp") ++ defines.map(define => s"-D$define") ++ List(header)))
          |  def released(headers: List[String], defines: List[String]): List[Int] =
          |    headers.map(header => release(List("cpp") ++ defines.map(define => s"-D$define") ++ List(header)))
          |  def listed(headers: List[String], defines: List[String]): java.util.ArrayList[List[String]] = {
          |    val kept = new java.util.ArrayList[List[String]]()
          |    headers.foreach { header =>
          |      val arguments = List("cpp") ++ defines.map(define => s"-D$define") ++ List(header)
          |      kept.add(arguments)
          |    }
          |    kept
          |  }
          |  def mapped(headers: List[String], defines: List[String]): java.util.HashMap[String, List[String]] = {
          |    val kept = new java.util.HashMap[String, List[String]]()
          |    headers.foreach { header =>
          |      val arguments = List("cpp") ++ defines.map(define => s"-D$define") ++ List(header)
          |      kept.put(header, arguments)
          |    }
          |    kept
          |  }
          |  def queued(headers: List[String], defines: List[String]): java.util.ArrayDeque[List[String]] = {
          |    val kept = new java.util.ArrayDeque[List[String]]()
          |    headers.foreach { header =>
          |      val arguments = List("cpp") ++ defines.map(define => s"-D$define") ++ List(header)
          |      kept.push(arguments)
          |    }
          |    kept
          |  }
          |  def folded(headers: List[String], defines: List[String]): List[String] =
          |    headers.foldLeft(List.empty[String])((all, header) => all ++ defines.map(define => s"-D$define$header"))
          |}""".stripMargin
      )
    )
    Complexity.findings(result.complexities, result.graph.nodes, result.hot)
  }

  private given Finding.Level = Finding.Level.Warning

  private def space(finding: Finding): Boolean = finding.rule.id == "space-complexity"

  test("a list built per iteration and handed to a call that drops it is not live across the loop") {
    assertFinding(found, "complexity", "Retention.passed is O(|defines| * |headers|) in time:")
    assertNoFinding(found)(finding => space(finding) && finding.detail.startsWith("Retention.passed "))
  }

  test("a list returned by a callee and handed to a call that drops it is not live across the loop") {
    assertFinding(found, "complexity", "Retention.delegated is O(|defines| * |headers|) in time:")
    assertNoFinding(found)(finding => space(finding) && finding.detail.startsWith("Retention.delegated "))
  }

  test("a callee in the build that does not keep its parameter leaves the list temporary") {
    assertFinding(found, "complexity", "Retention.released is O(|defines| * |headers|) in time:")
    assertNoFinding(found)(finding => space(finding) && finding.detail.startsWith("Retention.released "))
  }

  test("lists accumulated across iterations are live space") {
    assertFinding(found, "complexity", "Retention.flattened is O(|defines| * |headers|) in time and space")
    assertFinding(found, "complexity", "Retention.appended is O(|defines| * |headers|) in time and space")
    assertFinding(found, "complexity", "Retention.stored is O(|defines| * |headers|) in time and space")
    assertFinding(found, "complexity", "Retention.kept is O(|defines| * |headers|) in time and space")
    assertFinding(found, "complexity", "Retention.listed is O(|defines| * |headers|) in time and space")
    assertFinding(found, "complexity", "Retention.mapped is O(|defines| * |headers|) in time and space")
    assertFinding(found, "complexity", "Retention.queued is O(|defines| * |headers|) in time and space")
    assertFinding(found, "complexity", "Retention.folded is O(|defines| * |headers|^2) in time and space")(using
      Finding.Level.Error
    )
  }

  test("lists returned from the loop are live space") {
    assertFinding(found, "complexity", "Retention.returned is O(|defines| * |headers|) in time and space")
    assertFinding(found, "complexity", "Retention.delegatedReturn is O(|defines| * |headers|) in time and space")
  }
}
