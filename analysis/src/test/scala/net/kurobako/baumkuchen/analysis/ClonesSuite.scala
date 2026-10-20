package net.kurobako.baumkuchen.analysis

class ClonesSuite extends AnalysisSuite {
  private def findings(main: Seq[(String, String)], test: Seq[(String, String)] = Nil): Vector[Finding] = {
    val result = extract(main, test, generated = GeneratedSources)
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

  test("a copy of the rest of a for comprehension is found where it is, not at the generator before it") {
    def decoder(owner: String, head: String) =
      s"""object $owner {
         |  def decode(name: String, flag: Boolean, offset: Int): Either[String, Int] =
         |    for {
         |      access <- name match {
         |$head
         |        case _   => Left("none")
         |      }
         |      status <-
         |        if (!flag) Right(access * 2 + offset * 3 - 1)
         |        else Left(s"$$name status $$access").map(_ => access * 4 + offset * 5 - 7)
         |    } yield status
         |}""".stripMargin
    val found = findings(Seq(
      "First.scala" ->
        decoder("First", (1 to 20).map(i => s"""        case "a$i" => Right($i * offset)""").mkString("\n")),
      "Second.scala" -> decoder("Second", """        case "b" => Right(2)""")
    ))
    assertFinding(found, "duplicate-code", "2 copies of 4 lines", "First.scala:27-30", "Second.scala:8-11")
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

  test("a copy whose definition suppresses duplicate-code is not hashed") {
    val suppressed = average("Second", "values", "chosen", "twice")
      .replace("  def summary", "  @SuppressWarnings(Array(\"baumkuchen:duplicate-code\"))\n  def summary")
    assertEquals(
      findings(Seq("First.scala" -> average("First", "items", "kept", "scaled"), "Second.scala" -> suppressed)),
      Vector.empty
    )
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

  test("copies split between main code and tests, as an independent oracle is, are information") {
    val found = findings(
      Seq("Average.scala"       -> average("Average", "items", "kept", "scaled")),
      Seq("AverageOracle.scala" -> average("AverageOracle", "values", "chosen", "twice"))
    )
    assertFinding(found, "duplicate-code", "2 copies of", "(copies in main code and tests)")(using Finding.Level.Info)
  }

  test("a file repeating many fragments within itself is reported once, with its largest groups") {
    val bodies = Vector("size", "length", "sum", "max", "min", "product").flatMap(method =>
      Vector(1, 2).map(copy =>
        s"""  def $method$copy(items: List[Int], label: String): String = {
           |    val kept   = items.filter(_ > 0)
           |    val scaled = kept.map(value => value * 2 + 1)
           |    val total  = scaled.foldLeft(0)(_ + _)
           |    val count  = scaled.$method
           |    if (count == 0) label else s"$$label: $${total / count}"
           |  }""".stripMargin
      )
    )
    val found = findings(Seq("Table.scala" -> bodies.mkString("object Table {\n", "\n", "\n}")))
    assertFinding(found, "duplicate-code", "groups of copies repeat within", "Table.scala", "the largest at lines")
    assertEquals(found.size, 1, found.mkString("\n"))
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

  private def classMask(
      owner: String,
      shift: Int,
      mask: String,
      sign: String,
      infinite: Int = 9,
      shifting: String = ">>>"
  ) =
    s"""object $owner {
       |  def classMask(bits: Long): Long = {
       |    val exponent = (bits $shifting $shift) & $mask
       |    val negative = (bits & $sign) != 0L
       |    val classBit =
       |      if (exponent == $mask) if (negative) 2 else $infinite
       |      else if (exponent == 0) if (negative) 4 else 7
       |      else if (negative) 3 else 8
       |    1L << classBit
       |  }
       |}""".stripMargin

  private def near(found: Vector[Finding]): Vector[Finding] = found.filter(_.rule == Finding.Rule.NearDuplicateCode)

  test("copies that differ only in literals are near duplicates, with the literals as holes") {
    val found = findings(Seq(
      "Binary16.scala" -> classMask("Binary16", 10, "0x1f", "0x8000L"),
      "Binary32.scala" -> classMask("Binary32", 23, "0xff", "0x80000000L"),
      "Binary64.scala" -> classMask("Binary64", 52, "0x7ff", "0x8000000000000000L")
    ))
    assertFinding(
      found,
      "near-duplicate-code",
      "3 copies of",
      "differ only in literals {10, 23, 52}, {31, 255, 2047}, {32768L, 2147483648L, -9223372036854775808L}",
      "Binary16.scala",
      "Binary32.scala",
      "Binary64.scala",
      "one method taking the literals as parameters"
    )
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  test("copies that repeat exactly join near duplicates that differ in literals") {
    val found = findings(Seq(
      "First.scala"  -> classMask("First", 23, "0xff", "0x80000000L"),
      "Second.scala" -> classMask("Second", 23, "0xff", "0x80000000L"),
      "Third.scala"  -> classMask("Third", 10, "0x1f", "0x8000L")
    ))
    assertFinding(found, "duplicate-code", "2 copies of", "First.scala", "Second.scala")
    assertFinding(found, "near-duplicate-code", "3 copies of", "{23, 10}", "Third.scala")
  }

  test("copies that differ in more literals than their size carries are not near duplicates") {
    val found = findings(Seq(
      "Half.scala"   -> classMask("Half", 10, "0x1f", "0x8000L"),
      "Single.scala" -> classMask("Single", 23, "0xff", "0x80000000L", infinite = 1)
    ))
    assertEquals(found, Vector.empty)
  }

  private def classified(owner: String, shift: Int, exponent: String, fraction: String, sign: String, quiet: String) =
    s"""object $owner {
       |  def classMask(bits: Long): Long = {
       |    val exponent = (bits >>> $shift) & $exponent
       |    val fraction = bits & $fraction
       |    val negative = (bits & $sign) != 0L
       |    val classBit =
       |      if (exponent == $exponent && fraction != 0) if ((fraction & $quiet) == 0) 0 else 1
       |      else if (exponent == $exponent) if (negative) 2 else 9
       |      else if (exponent == 0 && fraction == 0) if (negative) 5 else 6
       |      else if (exponent == 0) if (negative) 4 else 7
       |      else if (negative) 3 else 8
       |    1L << classBit
       |  }
       |}""".stripMargin

  test("larger copies may differ in one literal for every sixteen nodes") {
    val found = findings(Seq(
      "Half.scala" ->
        classified("Half", shift = 10, exponent = "0x1f", fraction = "0x3ff", sign = "0x8000L", quiet = "0x200"),
      "Single.scala" -> classified(
        "Single",
        shift = 23,
        exponent = "0xff",
        fraction = "0x7fffff",
        sign = "0x80000000L",
        quiet = "0x400000"
      )
    ))
    assertFinding(found, "near-duplicate-code", "2 copies of", "{10, 23}", "{512, 4194304}")
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  private val Widths =
    """object Widths {
      |  def wrap(width: Int, bits: Long): Long = bits & ((1L << width) - 1)
      |  def minimum(left: Long, right: Long, width: Int): Long = math.min(left, right) >> width
      |}""".stripMargin

  private def minimum(width: Int) =
    s"  def minimum$width(x: Long, y: Long): Long = Widths.wrap(8, Widths.minimum(x + 1, y + 1, $width))"

  test("three one-line copies that differ only in literals are a family of near duplicates") {
    val found = findings(Seq(
      "Widths.scala"  -> Widths,
      "Minimum.scala" -> Seq(16, 32, 64).map(minimum(_)).mkString("object Minimum {\n", "\n", "\n}")
    ))
    assertFinding(found, "near-duplicate-code", "3 copies of 1 line ", "{16, 32, 64}")
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  test("overloads one per width, and inline methods specialising on literals, are near duplicates by design") {
    val overloads = Seq(16, 32, 64).map(width =>
      s"  def minimum(value: F$width): Long = Widths.wrap(8, Widths.minimum(value.hashCode + 1L, value.hashCode + 2L, $width))"
    )
    val staged = Seq(16, 32, 64).map(width =>
      s"  inline def staged$width(x: Long): Long = lane(8) + Widths.wrap(8, Widths.minimum(x + 1, 2L, $width))"
    )
    val found = findings(Seq(
      "Widths.scala"    -> Widths,
      "Overloads.scala" ->
        (Seq(16, 32, 64).map(width => s"case class F$width(bits: Long)").mkString("\n") +
          overloads.mkString("\nobject Overloads {\n", "\n", "\n}")),
      "Staged.scala" ->
        ("object Staged {\n  inline def lane(inline width: Int): Long = Widths.wrap(width, 1L)\n" +
          staged.mkString("\n") + "\n}")
    ))
    assertNoFinding(found)(_.rule == Finding.Rule.NearDuplicateCode)
  }

  test("values initialised alike but for numbers are not a family of near duplicates") {
    val arrays = Seq(16, 32, 64).map(size =>
      s"  val buffer$size: Array[Long] = Array.ofDim[Long]($size + Widths.wrap(8, 2L).toInt)"
    )
    assertEquals(
      findings(Seq("Widths.scala" -> Widths, "Buffers.scala" -> arrays.mkString("object Buffers {\n", "\n", "\n}"))),
      Vector.empty
    )
  }

  test("two one-line copies that differ only in literals are not near duplicates") {
    assertEquals(
      findings(Seq(
        "Widths.scala"  -> Widths,
        "Minimum.scala" -> Seq(16, 32).map(minimum(_)).mkString("object Minimum {\n", "\n", "\n}")
      )),
      Vector.empty
    )
  }

  test("three one-line copies that differ only in strings are a table, not a family") {
    val labelled = (name: String) =>
      s"""  def $name(x: Long, y: Long): String = if (x > y) Names.label(8, Names.join(x, y, "$name")) else "$name""""
    val names =
      """object Names {
        |  def label(width: Int, text: String): String = text.take(width)
        |  def join(left: Long, right: Long, name: String): String = s"$name$left$right"
        |}""".stripMargin
    assertEquals(
      findings(Seq(
        "Names.scala"  -> names,
        "Labels.scala" -> Seq("first", "second", "third").map(labelled).mkString("object Labels {\n", "\n", "\n}")
      )),
      Vector.empty
    )
  }

  test("small copies that differ only in literals are not near duplicates") {
    val clamp = (owner: String, mask: String) =>
      s"""object $owner {
         |  def clamp(left: Long, right: Long): Long =
         |    math.min(left & $mask, right & $mask) << 1
         |}""".stripMargin
    assertEquals(
      findings(Seq("Half.scala" -> clamp("Half", "0xffffL"), "Single.scala" -> clamp("Single", "0xffffffffL"))),
      Vector.empty
    )
  }

  test("exact duplicates are not near duplicates") {
    val found = findings(Seq(
      "First.scala"  -> classMask("First", 23, "0xff", "0x80000000L"),
      "Second.scala" -> classMask("Second", 23, "0xff", "0x80000000L")
    ))
    assertFinding(found, "duplicate-code", "2 copies of")
    assertEquals(near(found), Vector.empty)
  }

  test("copies that also differ in a call are not near duplicates") {
    assertEquals(
      findings(Seq(
        "Half.scala"   -> classMask("Half", 10, "0x1f", "0x8000L"),
        "Single.scala" -> classMask("Single", 23, "0xff", "0x80000000L", shifting = ">>")
      )),
      Vector.empty
    )
  }

  test("calls of one function that differ only in the literals they pass are not near duplicates") {
    val call = (owner: String, id: String) =>
      s"""object $owner {
         |  def reported(x: Int, y: Int): String =
         |    Shared.report(
         |      s"$id-identifier $${x + y}",
         |      "the message the $id call reports",
         |      ((x + 2) * (y + 4) - (x + 6)) * ((y + 8) - (x + 10)) + x * y,
         |      (y * 12 - (x + 14) * (y - 16)) / ((x * 18) + 20 - y)
         |    )
         |}""".stripMargin
    val shared =
      """object Shared { def report(id: String, message: String, first: Int, second: Int): String = s"$id $message" }"""
    assertEquals(
      near(findings(Seq(
        "Shared.scala" -> shared,
        "First.scala"  -> call("First", "first"),
        "Second.scala" -> call("Second", "second")
      ))),
      Vector.empty
    )
  }
}
