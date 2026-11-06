package net.kurobako.bonsai.analysis

import java.util.concurrent.atomic.AtomicInteger

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

  test("every copy of a group is listed with its file and lines, however many there are") {
    val names = Vector("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh")
    val found = findings(names.map(name => s"$name.scala" -> average(name, "items", "kept", "scaled")))
    assertEquals(found.size, 1, found.mkString("\n"))
    assertFinding(found, "duplicate-code", "7 copies of")
    names.foreach(name => assert(found.head.detail.contains(s"$name.scala:"), found.head.detail))
    assert(!found.head.detail.contains("more"), found.head.detail)
    assertEquals(found.head.detail.split(", ").count(_.contains(".scala:")), 7, found.head.detail)
  }

  test("copies in intended pairs are left out, and a group with another copy is still reported") {
    def reported(names: Seq[String], pairs: (String, String)*) = {
      val result = extract(
        names.map(name => s"$name.scala" -> average(name, "items", "kept", "scaled")),
        generated = GeneratedSources
      )
      Clones.findings(result.clones, result.graph.nodes, Intended(pairs.toVector)).size
    }
    val two = Seq("Execute", "Render")
    assertEquals(reported(two), 1)
    assertEquals(reported(two, "Execute" -> "Render"), 0)
    assertEquals(reported(two, "Render" -> "Execute"), 0)
    assertEquals(reported(two, "Exec*" -> "R*"), 0)
    assertEquals(reported(two, "Execute" -> "Lower"), 1)
    assertEquals(reported(two, "Execute" -> "Execute"), 1)
    val three = two :+ "Lower"
    assertEquals(reported(three, "Execute" -> "Render"), 1)
    assertEquals(reported(three, "Execute" -> "Render", "Execute" -> "Lower"), 1)
    assertEquals(reported(three, "Execute" -> "Render", "Execute" -> "Lower", "Render" -> "Lower"), 0)
  }

  test("near duplicates that are an intended pair are left out too") {
    val sources = Seq(
      "Binary16.scala" -> classMask("Binary16", 10, "0x1f", "0x8000L"),
      "Binary32.scala" -> classMask("Binary32", 23, "0xff", "0x80000000L"),
      "Binary64.scala" -> classMask("Binary64", 52, "0x7ff", "0x8000000000000000L")
    )
    val result                             = extract(sources, generated = GeneratedSources)
    def reported(pairs: (String, String)*) =
      Clones.findings(result.clones, result.graph.nodes, Intended(pairs.toVector)).map(_.rule.id)
    assertEquals(reported(), Vector("near-duplicate-code"))
    assertEquals(reported("Binary16" -> "Binary32"), Vector("near-duplicate-code"))
    assertEquals(reported("Binary16" -> "Binary32", "Binary16" -> "Binary64"), Vector("near-duplicate-code"))
    assertEquals(
      reported("Binary16" -> "Binary32", "Binary16" -> "Binary64", "Binary32" -> "Binary64"),
      Vector.empty
    )
    assertEquals(reported("Binary*" -> "Binary*"), Vector.empty)
  }

  test("dense expressions repeated across files are duplicates, even when short") {
    val found = findings(Seq(
      "Factors.scala" ->
        """object Factors {
                           |  def maximal(frequent: Vector[(Set[String], Set[Int])]): Vector[(Set[String], Set[Int])] =
                           |    frequent.groupMap(_._2)(_._1).toVector.flatMap { (cases, candidates) =>
                           |      candidates.filterNot(fields => candidates.exists(other => other.size > fields.size && fields.subsetOf(other) && other.nonEmpty && fields.nonEmpty && other.size < 100))
                           |        .map(_ -> cases)
                           |    }
                           |}""".stripMargin,
      "Clumps.scala" ->
        """object Clumps {
                          |  def clumps(frequent: Vector[(Set[String], Set[Int])]): Vector[(Set[String], Int)] =
                          |    frequent.groupMap(_._2)(_._1).toVector.flatMap { (methods, candidates) =>
                          |      candidates.filterNot(items => candidates.exists(other => other.size > items.size && items.subsetOf(other) && other.nonEmpty && items.nonEmpty && other.size < 100))
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
      .replace("  def summary", "  @SuppressWarnings(Array(\"bonsai:duplicate-code\"))\n  def summary")
    assertEquals(
      findings(Seq("First.scala" -> average("First", "items", "kept", "scaled"), "Second.scala" -> suppressed)),
      Vector.empty
    )
  }

  test("copies that are all generated are not reported, while a handwritten copy among them keeps the finding") {
    val sources = Seq(
      "First.scala"  -> average("First", "items", "kept", "scaled"),
      "Second.scala" -> average("Second", "values", "chosen", "twice")
    )
    assertEquals(findings(sources.map((name, code) => s"src/generated/$name" -> code)), Vector.empty)
    val mixed = findings(Seq(sources.head, s"src/generated/${sources(1)._1}" -> sources(1)._2))
    assertFinding(mixed, "duplicate-code", "2 copies of")
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
      shifting: String = ">>>",
      low: Int = 3,
      last: String = "<<"
  ) =
    s"""object $owner {
       |  def classMask(bits: Long): Long = {
       |    val exponent = (bits $shifting $shift) & $mask
       |    val negative = (bits & $sign) != 0L
       |    val classBit =
       |      if (exponent == $mask) if (negative) 2 else $infinite
       |      else if (exponent == 0) if (negative) 4 else 7
       |      else if (negative) $low else 8
       |    1L $last classBit
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
    assertFinding(
      found,
      "near-duplicate-code",
      "3 copies of",
      "{23, 10}",
      "First.scala",
      "Second.scala",
      "Third.scala"
    )
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  test("many copies that differ only in literals are a table of rows, not near duplicates") {
    def rows(count: Int) = (1 to count).map(row =>
      s"Row$row.scala" -> classMask(s"Row$row", row + 100, "0xff", "0x80000000L")
    )
    assertEquals(near(findings(rows(32))).size, 1)
    assertEquals(near(findings(rows(33))), Vector.empty)
  }

  test("copies that differ in more literals than their size carries are not near duplicates") {
    val found = findings(Seq(
      "Half.scala"   -> classMask("Half", 10, "0x1f", "0x8000L"),
      "Single.scala" -> classMask("Single", 23, "0xff", "0x80000000L", infinite = 1, low = 5)
    ))
    assertEquals(found, Vector.empty)
  }

  private def classified(owner: String, shift: Int, exponent: String, fraction: String, sign: String, quiet: String) =
    s"""object $owner {
       |  def classMask(bits: Long): Long = {
       |    val exponent = (bits >>> $shift) & $exponent
       |    val fraction = bits & $fraction
       |    val negative = (bits & $sign) != 0L
       |    val spread = (exponent + fraction) * (exponent - fraction) + exponent * fraction
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

  test("three one-line copies that differ only in literals are a table, not a family of near duplicates") {
    val found = findings(Seq(
      "Widths.scala"  -> Widths,
      "Minimum.scala" -> Seq(16, 32, 64).map(minimum(_)).mkString("object Minimum {\n", "\n", "\n}")
    ))
    assertEquals(found, Vector.empty)
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
        "Single.scala" -> classMask("Single", 23, "0xff", "0x80000000L", shifting = ">>", last = ">>")
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

  test("near duplicates on Map literals do not suggest parameterising literals into a method") {
    def make(owner: String, a: Int, b: Int, c: Int, d: Int): String =
      s"""object $owner {
         |  def table(): Map[String, Int] = {
         |    val map = Map(
         |      "alpha" -> $a,
         |      "beta"  -> $b,
         |      "gamma" -> $c,
         |      "delta" -> $d,
         |      "extra1" -> ($a + 1),
         |      "extra2" -> ($b + 2),
         |      "extra3" -> ($c + 3),
         |      "extra4" -> ($d + 4),
         |      "extra5" -> ($a + $b),
         |      "extra6" -> ($c + $d)
         |    )
         |    map
         |  }
         |}""".stripMargin
    val found = findings(Seq(
      "First.scala"  -> make("First", 1, 2, 3, 4),
      "Second.scala" -> make("Second", 10, 20, 30, 40)
    ))
    val clones = near(found)
    assertEquals(clones.size, 1)
    assert(!clones.head.detail.contains("replace them with one method taking the literals as parameters"))
    assert(clones.head.detail.contains("share a common lookup table or keep them distinct"))
  }

  test("a case pattern repeated across matches is not a duplicate, its bodies are") {
    val pattern = (owner: String, body: String) =>
      s"""object $owner {
         |  def run(instruction: Instruction): Long = instruction match {
         |    case Instruction.Copy(
         |          resultType = resultType,
         |          dest = destination,
         |          src = source,
         |          count = count,
         |          stride = stride,
         |          offset = offset,
         |          mask = mask,
         |          scope = scope,
         |          tile = tile,
         |          group = group,
         |          lane = lane,
         |          slot = slot
         |        ) => $body
         |    case _ => 0L
         |  }
         |}""".stripMargin
    val instruction =
      "sealed trait Instruction\nobject Instruction { case class Copy(resultType: Int, dest: Int, src: Int, count: Int, stride: Int, offset: Int, mask: Int, scope: Int, tile: Int, group: Int, lane: Int, slot: Int) extends Instruction }"
    val found = findings(Seq(
      "Instruction.scala" -> instruction,
      "Semantics.scala"   ->
        pattern("Semantics", "resultType + destination + source + count + stride + offset + mask + scope"),
      "Lowering.scala" -> pattern("Lowering", "resultType * destination - source")
    ))
    assertEquals(found.filter(_.rule == Finding.Rule.DuplicateCode), Vector.empty, found.mkString("\n"))
    val repeated = findings(Seq(
      "Instruction.scala" -> instruction,
      "Semantics.scala"   ->
        pattern("Semantics", "resultType + destination + source + count + stride + offset + mask + scope"),
      "Lowering.scala" ->
        pattern("Lowering", "resultType + destination + source + count + stride + offset + mask + scope")
    ))
    assertFinding(repeated, "duplicate-code", "Semantics.scala", "Lowering.scala")
  }
  test("single calls in table rows that differ only in their arguments are not near duplicates") {
    def rows(clamp: String, limit: Int): String =
      s"""import Table.value
         |object Rules$clamp {
         |  val rules = Vector(
         |    value(Vector("bmsk"), 2, "range", Vector(0, $limit), "the cited sentence", Vector("$clamp")),
         |    value(Vector("cvt"), 3, "range", Vector(1, $limit), "the other sentence", Vector("$clamp")),
         |    value(Vector("pack"), 4, "range", Vector(2, $limit), "a third sentence", Vector("$clamp"))
         |  )
         |}""".stripMargin
    val table =
      """object Table {
        |  def value(names: Vector[String], bits: Int, kind: String, range: Vector[Int], cite: String, all: Vector[String]): String = cite
        |}""".stripMargin
    assertEquals(
      near(findings(Seq("Table.scala" -> table, "Clamp.scala" -> rows("clamp", 32), "Wrap.scala" -> rows("wrap", 31)))),
      Vector.empty
    )
  }

  test("per-width members of companions that differ only in the width are near duplicates by design") {
    def companion(width: Int): String =
      s"""object Float$width {
         |  def add(l: Long, r: Long): Long = FloatingPoint(
         |    IEEE754.add(l, r, $width) + IEEE754.add(r, l, $width) + IEEE754.add(l, l, $width) + IEEE754.add(r, r, $width)
         |  )
         |}""".stripMargin
    val ieee     = "object IEEE754 { def add(l: Long, r: Long, width: Int): Long = l + r + width }"
    val floating = "object FloatingPoint { def apply(bits: Long): Long = bits }"
    val found    = near(findings(Seq(
      "IEEE754.scala"       -> ieee,
      "FloatingPoint.scala" -> floating,
      "Float16.scala"       -> companion(16),
      "Float32.scala"       -> companion(32),
      "Float64.scala"       -> companion(64)
    )))
    assertEquals(found, Vector.empty, found.mkString("\n"))
    val shared = near(findings(Seq(
      "IEEE754.scala"       -> ieee,
      "FloatingPoint.scala" -> floating,
      "Floats.scala"        ->
        """object Floats {
          |  def add16(l: Long, r: Long): Long = FloatingPoint(
          |    IEEE754.add(l, r, 16) + IEEE754.add(r, l, 16) + IEEE754.add(l, l, 16) + IEEE754.add(r, r, 16)
          |  )
          |  def add32(l: Long, r: Long): Long = FloatingPoint(
          |    IEEE754.add(l, r, 32) + IEEE754.add(r, l, 32) + IEEE754.add(l, l, 32) + IEEE754.add(r, r, 32)
          |  )
          |  def add64(l: Long, r: Long): Long = FloatingPoint(
          |    IEEE754.add(l, r, 64) + IEEE754.add(r, l, 64) + IEEE754.add(l, l, 64) + IEEE754.add(r, r, 64)
          |  )
          |}""".stripMargin
    )))
    assertFinding(shared, "near-duplicate-code", "3 copies of", "{16, 32, 64}")
  }

  test("enum cases that differ only in their constructor arguments are not near duplicates") {
    val found = findings(Seq(
      "Format.scala" ->
        """enum Format(
          |    val shader: Shader,
          |    val texelBytes: Int,
          |    val depth: Depth,
          |    val stencilOffset: Int,
          |    val compression: Compression,
          |    val srgb: Boolean,
          |    val blockWidth: Int,
          |    val blockHeight: Int
          |) {
          |  case Etc2R8g8b8UnormBlock extends Format(
          |    Shader.Decoded,
          |    8,
          |    Depth.None,
          |    -1,
          |    Compression.Etc,
          |    false,
          |    4,
          |    4
          |  )
          |  case Etc2R8g8b8SrgbBlock extends Format(
          |    Shader.Decoded,
          |    8,
          |    Depth.None,
          |    -1,
          |    Compression.Etc,
          |    true,
          |    4,
          |    4
          |  )
          |  case Etc2R8g8b8a1UnormBlock extends Format(
          |    Shader.Rgba,
          |    8,
          |    Depth.None,
          |    -1,
          |    Compression.Etc,
          |    false,
          |    4,
          |    4
          |  )
          |  case Etc2R8g8b8a1SrgbBlock extends Format(
          |    Shader.Rgba,
          |    8,
          |    Depth.None,
          |    -1,
          |    Compression.Etc,
          |    true,
          |    4,
          |    4
          |  )
          |}
          |enum Shader { case Decoded, Rgba }
          |enum Depth { case None }
          |enum Compression { case Etc }""".stripMargin
    ))
    assertEquals(near(found), Vector.empty, found.mkString("\n"))
  }

  test("methods with copied logic in one file are still duplicates") {
    val found = findings(Seq(
      "Pair.scala" ->
        """object Pair {
          |  def first(items: List[Int]): Int = {
          |    val kept = items.filter(_ > 0)
          |    val doubled = kept.map(value => value * 2 + 1)
          |    doubled.foldLeft(0)(_ + _) + doubled.size
          |  }
          |  def second(items: List[Int]): Int = {
          |    val kept = items.filter(_ > 0)
          |    val doubled = kept.map(value => value * 2 + 1)
          |    doubled.foldLeft(0)(_ + _) + doubled.size
          |  }
          |}""".stripMargin
    ))
    assertFinding(found, "duplicate-code", "2 copies of", "Pair.scala:2-6", "Pair.scala:7-11")
    assertEquals(found.size, 1, found.mkString("\n"))
  }

  test("a duplicate is anchored at the first line of its first copy") {
    val found = findings(Seq(
      "Pair.scala" ->
        """object Pair {
          |  def first(items: List[Int]): Int = {
          |    val kept = items.filter(_ > 0)
          |    val doubled = kept.map(value => value * 2 + 1)
          |    doubled.foldLeft(0)(_ + _) + doubled.size
          |  }
          |  def second(items: List[Int]): Int = {
          |    val kept = items.filter(_ > 0)
          |    val doubled = kept.map(value => value * 2 + 1)
          |    doubled.foldLeft(0)(_ + _) + doubled.size
          |  }
          |}""".stripMargin
    ))
    assertEquals(found.map(_.node.line), Vector(2), found.mkString("\n"))
  }

  test("a literal is rendered when a finding names it, not when it is built") {
    val renderings = AtomicInteger()
    val literal    = Clones.Literal("a", string = true) { renderings.incrementAndGet(); "\"a\"" }
    assertEquals(renderings.get, 0)
    assertEquals(literal.shown, "\"a\"")
    assertEquals(literal.shown, "\"a\"")
    assertEquals(renderings.get, 1)
  }

  test("a deferred value is computed once, when first used") {
    val computations = AtomicInteger()
    val deferred     = Clones.Deferred { computations.incrementAndGet(); Vector(1) }
    assertEquals(computations.get, 0)
    assertEquals(deferred.value, Vector(1))
    assertEquals(deferred.value, Vector(1))
    assertEquals(computations.get, 1)
  }

  test("copies across modules are reported only where some module they all depend on could hold the code") {
    def node(name: String, module: String) = name ->
      Node(name, module, Output.Scope.Main, s"$name.scala", 1, generated = false, exported = false, outer = None)
    val nodes = Map(node("App", "app"), node("Lib", "lib"), node("Other", "other"), node("Base", "base"))
    def group(owners: String*) =
      Clones.Group(40, owners.toVector.map(Clones.Fragment(_, "x.scala", start = 1, end = 3, test = false)))
    def reported(edges: Map[String, Set[String]], owners: String*) =
      Clones.findings(Vector(group(owners*)), nodes, edges = edges).size
    val shared = Map("App" -> Set("Base"), "Lib" -> Set("Base"), "Other" -> Set.empty[String])
    assertEquals(reported(shared, "App", "Lib"), 1)
    assertEquals(reported(shared, "App", "Other"), 0)
    assertEquals(reported(Map("App" -> Set("Lib")), "App", "Lib"), 1)
    assertEquals(reported(Map("App" -> Set("Lib"), "Lib" -> Set("Base")), "App", "Base"), 1)
    assertEquals(reported(Map.empty, "App", "Lib"), 0)
    assertEquals(reported(Map.empty, "App", "App"), 1)
  }
}
