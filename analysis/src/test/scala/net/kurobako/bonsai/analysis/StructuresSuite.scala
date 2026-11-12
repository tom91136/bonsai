package net.kurobako.bonsai.analysis

class StructuresSuite extends AnalysisSuite {
  private def findings(
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      twins: Seq[Seq[(String, String)]] = Nil,
      generated: Vector[String] = Vector.empty,
      libraries: Set[String] = Set.empty
  ): Vector[Finding] = {
    val result = extract(main, test, twins = twins, generated = generated, libraries = libraries)
    Structures.findings(result.structures, result.graph)
  }

  private def of(found: Vector[Finding], rule: Finding.Rule): Vector[String] =
    found.view.collect { case finding if finding.rule == rule => finding.detail.takeWhile(_ != ';') }.toVector.sorted

  test("a string naming a member the type spells as a string itself is a name the type reports, not a stale member") {
    val found = findings(
      Seq(
        "Linter.scala" ->
          """object Linter {
            |  def report: String = diagnostic("unqualified")
            |  private def diagnostic(id: String): String = id
            |}""".stripMargin
      ),
      Seq("LinterTest.scala" -> """object LinterTest { val ids = Vector("Linter.unqualified", "Linter.missing") }""")
    )
    assertEquals(
      of(found, Finding.Rule.StaleNameString),
      Vector("\"Linter.missing\" names Linter.missing, which no Linter of the build has")
    )
  }

  test("a field named like its class reads twice wherever a value is named for the type") {
    val found = findings(Seq(
      "Model.scala" ->
        """case class Graph(graph: Map[String, Int])
          |class Nodes(val nodes: Vector[Int])
          |class Holder(holder: Int) { def value: Int = holder }
          |case class Edge(from: String)
          |object Use { def size(graph: Graph): Int = graph.graph.size + graph.graph.size }""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.FieldStutter),
      Vector(
        "Graph.graph is named like Graph, so code reads graph.graph, as 2 places do",
        "Nodes.nodes is named like Nodes, so code reads nodes.nodes"
      )
    )
  }

  test("a string naming a member its type no longer has is left from a rename") {
    val found = findings(
      Seq(
        "Tools.scala" ->
          """enum Color { case Red, Green }
            |object Tools {
            |  def renamed: Int = 1
            |  val notes = Vector(
            |    "Tools.original", "Tools.renamed", "Tools.renamed(1)", "Tools.original is gone", "Other.thing",
            |    "Color.Red", "Tools.scala", "Tools.toString", "Color.Blue("
            |  )
            |}""".stripMargin
      ),
      Seq("ToolsTest.scala" -> """object ToolsTest { val name = "Tools.removed" }""")
    )
    assertEquals(
      of(found, Finding.Rule.StaleNameString),
      Vector(
        "\"Color.Blue(\" names Color.Blue, which no Color of the build has",
        "\"Tools.original\" names Tools.original, which no Tools of the build has",
        "\"Tools.removed\" names Tools.removed, which no Tools of the build has"
      )
    )
    assertFinding(found, "stale-name-string", "Tools.removed")(using Finding.Level.Info)
    assertFinding(found, "stale-name-string", "Tools.original")
  }

  test("a string elsewhere does not name a type nested in another type, which only that type names simply") {
    val found = findings(
      Seq(
        "Trips.scala" ->
          """object Trips {
            |  private case class Render(parts: Vector[String])
            |  private def size(render: Render): Int = render.parts.size
            |  val count: Int = size(Render(Vector.empty))
            |}""".stripMargin
      ),
      Seq("RenderTest.scala" -> """object RenderTest { val name = "Render.render(text, width)" }""")
    )
    assertEquals(of(found, Finding.Rule.StaleNameString), Vector.empty)
  }

  test("a literal completing a name that the splice before it begins is not a stale name") {
    val found = findings(Seq(
      "Images.scala" ->
        """object Images {
          |  class ImageFormat
          |  class IntegerImageFormat { object Layout }
          |  def render(family: String, args: String): String = s"Some(${family}ImageFormat.Layout($args))"
          |  def bare(args: String): String = "ImageFormat.Layout(" + args
          |}""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.StaleNameString),
      Vector("\"ImageFormat.Layout(\" names ImageFormat.Layout, which no ImageFormat of the build has")
    )
  }

  test("aliases filling one constructor with literals give each type a second spelling") {
    val found = findings(Seq(
      "Widths.scala" ->
        """class Bits[N <: Int]
          |object Widths {
          |  type Bits8  = Bits[8]
          |  type Bits16 = Bits[16]
          |  type Bits32 = Bits[32]
          |  type Pair1 = Bits[1]
          |  type Code = Bits[4]
          |  object Code { def zero: Int = 0 }
          |}
          |object Others { type Pair2 = Bits[2]; type Pair3 = Bits[3]; type Plain = Bits[Int] }""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.ParameterAliasFamily),
      Vector("4 aliases fill Bits with a literal (Bits16, Bits32, Bits8, Pair1), giving each type a second spelling")
    )
  }

  private val core = Seq(
    "A.scala" -> "package app\nclass A { def b: B = B() }",
    "B.scala" -> "package app\nclass B { def c: C = C() }",
    "C.scala" -> "package app\nclass C { def a: A = A() }"
  )

  private val ui = Seq(
    "X.scala" -> "package app\nclass X { def y: Y = Y(); def a: A = A() }",
    "Y.scala" -> "package app\nclass Y { def z: Z = Z() }",
    "Z.scala" -> "package app\nclass Z { def x: X = X() }"
  )

  test("files of a package that depend on the rest, never the other way, can be a subpackage") {
    assertEquals(
      of(findings(core ++ ui), Finding.Rule.PackageSplitCandidate),
      Vector(
        "in app, 3 files (X.scala, Y.scala, Z.scala) depend on 3 others through 1 reference, which never depend back"
      )
    )
    val tangled = core.tail :+ ("A.scala" -> "package app\nclass A { def b: B = B(); def x: X = X() }")
    assertEquals(of(findings(tangled ++ ui), Finding.Rule.PackageSplitCandidate), Vector.empty)
    val leaning = ui.map((name, source) => name -> source.replace(" }", "; def all = (A(), B(), C()) }"))
    assertEquals(of(findings(core ++ leaning), Finding.Rule.PackageSplitCandidate), Vector.empty)
  }

  test("files of a package that hold an entry point are not offered a subpackage") {
    val launched = ui.updated(
      0,
      "X.scala" ->
        "package app\nclass X { def y: Y = Y(); def a: A = A() }\nobject Launch { def main(args: Array[String]): Unit = () }"
    )
    assertEquals(of(findings(core ++ launched), Finding.Rule.PackageSplitCandidate), Vector.empty)
  }

  test("a concrete class only its one subclass extends and nothing constructs is that subclass") {
    val found = findings(
      Seq(
        "Classes.scala" ->
          """class Base { def f: Int = 1 }
            |class Only extends Base { def g: Int = 2 }
            |class Built { def f: Int = 1 }
            |class Child extends Built
            |class Shared { def f: Int = 1 }
            |class Lhs extends Shared
            |class Rhs extends Shared
            |class Hooked { def f: Int = 1 }
            |abstract class Shape { def area: Int }
            |class Square extends Shape { def area: Int = 4 }
            |class Doubled { def f: Int = 1 }
            |object Main {
            |  def main(args: Array[String]): Unit =
            |    println(Seq(Only(), Built(), Child(), Lhs(), Rhs(), new Hooked { override def f = 2 }, Square()))
            |}""".stripMargin
      ),
      Seq("Doubles.scala" -> "class FakeDoubled extends Doubled")
    )
    assertEquals(
      of(found, Finding.Rule.BaseClassSingleSubclass),
      Vector("Base is never constructed itself and only Only extends it")
    )
  }

  test("an object whose members fall into groups that never meet holds two designs") {
    val found = findings(Seq(
      "Kinds.scala" -> "case class Token(text: String)\ncase class Page(lines: Vector[String])",
      "Util.scala"  ->
        """object Util {
          |  def parse(text: String): Token        = Token(text)
          |  def parseAll(texts: Vector[String])  = texts.map(parse)
          |  def render(page: Page): String       = page.lines.mkString
          |  def renderAll(pages: Vector[Page])   = pages.map(render)
          |  def lone: Int                        = 1
          |}
          |object Joined {
          |  def parse(text: String): Token  = Token(text)
          |  def parseAll(texts: Vector[String]) = texts.map(parse)
          |  def paged(token: Token): Page   = Page(Vector(token.text))
          |  def pagedAll(tokens: Vector[Token]) = tokens.map(paged)
          |}""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.DisjointObject),
      Vector(
        "object Util holds 2 groups of members that never use each other and use different types " +
          "({parse, parseAll}, {render, renderAll})"
      )
    )
  }

  test("private vals of different types, each used by its own methods, hold two designs") {
    val found = findings(Seq(
      "Kinds.scala"  -> "case class Token(text: String)\ncase class Page(lines: Vector[String])",
      "Codecs.scala" ->
        """object Codecs {
          |  private val tokenDefault = Token("")
          |  private val pageDefault  = Page(Vector.empty)
          |  def readToken(text: String): Token = tokenDefault.copy(text = text)
          |  def readTokens(texts: Vector[String]) = texts.map(readToken)
          |  def pageSize(page: Page): Int = pageDefault.lines.size + page.lines.size
          |  def pageSizes(pages: Vector[Page]) = pages.map(pageSize)
          |}""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.DisjointObject),
      Vector(
        "object Codecs holds 2 groups of members that never use each other and use different types " +
          "({pageDefault, pageSize, pageSizes}, {readToken, readTokens, tokenDefault})"
      )
    )
  }

  test("an enumeration is never split into the values and methods that use each type") {
    val found = findings(Seq(
      "Kinds.scala"        -> "case class Token(text: String)\ncase class Page(lines: Vector[String])",
      "PortProtocol.scala" ->
        """object PortProtocol extends Enumeration {
          |  val TCP, UDP = Value
          |  def parse(text: String): Token = Token(text)
          |  def parseAll(texts: Vector[String]) = texts.map(parse)
          |  def render(page: Page): String = page.lines.mkString
          |  def renderAll(pages: Vector[Page]) = pages.map(render)
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("constants are not groups of members that use different types") {
    val found = findings(Seq(
      "Discriminants.scala" ->
        """object Discriminants {
          |  val NullDiscriminant     = -1
          |  val FiniteDiscriminant   = 0
          |  val InfiniteDiscriminant = 1
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("groups of methods that take and return String, without calling each other, are not split") {
    val found = findings(Seq(
      "Text.scala" ->
        """object Text {
          |  def trimmed(text: String): String = text.take(3)
          |  def padded(text: String): String  = trimmed(text).reverse
          |  def upper(text: String): String   = text.capitalize
          |  def loud(text: String): String    = upper(text).reverse
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("groups that each use a different library type hold two designs") {
    val found = findings(Seq(
      "Scanning.scala" ->
        """import java.nio.file.Path
          |import java.util.regex.Pattern
          |
          |object Scanning {
          |  def keep(regex: Pattern): Pattern   = regex
          |  def retain(regex: Pattern): Pattern = keep(regex)
          |  def locate(path: Path): Path        = path
          |  def resolve(path: Path): Path       = locate(path)
          |}""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.DisjointObject),
      Vector(
        "object Scanning holds 2 groups of members that never use each other and use different types " +
          "({keep, retain}, {locate, resolve})"
      )
    )
  }

  test("groups that share a library type only through a collection of it are not split") {
    val found = findings(Seq(
      "Gathering.scala" ->
        """import java.nio.file.Path
          |
          |object Gathering {
          |  def locate(path: Path): Path         = path
          |  def resolve(path: Path): Path        = locate(path)
          |  def gather(paths: Vector[Path]): Int = paths.size
          |  def count(paths: Vector[Path]): Int  = gather(paths) + 1
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("groups that share only a collection, but of different types, hold two designs") {
    val found = findings(Seq(
      "Batches.scala" ->
        """import java.nio.file.Path
          |import java.util.regex.Pattern
          |
          |object Batches {
          |  def keep(paths: Vector[Path]): Int          = paths.size
          |  def retain(paths: Vector[Path]): Int        = keep(paths) + 1
          |  def scan(patterns: Vector[Pattern]): Int    = patterns.size
          |  def scanAll(patterns: Vector[Pattern]): Int = scan(patterns) + 1
          |}""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.DisjointObject),
      Vector(
        "object Batches holds 2 groups of members that never use each other and use different types " +
          "({keep, retain}, {scan, scanAll})"
      )
    )
  }

  test("groups whose types are only Int, String and a Map of them are not split") {
    val found = findings(Seq(
      "Counting.scala" ->
        """object Counting {
          |  def width(text: String): Int               = text.length
          |  def widths(texts: Vector[String]): Int     = texts.size + width(texts.head)
          |  def tally(counts: Map[String, Int]): Int   = counts.size
          |  def tallies(counts: Map[String, Int]): Int = tally(counts) + 1
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("a group of only String and primitive types beside a group of a domain type is not split") {
    val found = findings(Seq(
      "Kinds.scala" -> "case class Token(text: String)",
      "Util.scala"  ->
        """object Util {
          |  def width(text: String): Int           = text.length
          |  def widths(texts: Vector[String]): Int = texts.size + width(texts.head)
          |  def parse(text: String): Token         = Token(text)
          |  def parseAll(texts: Vector[String])    = texts.map(parse)
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("a group of only String and primitive types does not hold back two groups of different domain types") {
    val found = findings(Seq(
      "Kinds.scala" -> "case class Token(text: String)\ncase class Page(lines: Vector[String])",
      "Util.scala"  ->
        """object Util {
          |  def width(text: String): Int        = text.length
          |  def widths(texts: Vector[String])   = texts.map(width)
          |  def parse(text: String): Token      = Token(text)
          |  def parseAll(texts: Vector[String]) = texts.map(parse)
          |  def render(page: Page): String      = page.lines.mkString
          |  def renderAll(pages: Vector[Page])  = pages.map(render)
          |}""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.DisjointObject),
      Vector(
        "object Util holds 2 groups of members that never use each other and use different types " +
          "({parse, parseAll}, {render, renderAll})"
      )
    )
  }

  test("groups that share only a tuple of distinct domain types hold two designs") {
    val found = findings(Seq(
      "Kinds.scala" -> "case class Token(text: String)\ncase class Page(lines: Vector[String])",
      "Pairs.scala" ->
        """object Pairs {
          |  def first(pair: (Token, Int)): Token   = pair._1
          |  def firsts(pairs: Vector[(Token, Int)]) = pairs.map(first)
          |  def second(pair: (Page, Int)): Page    = pair._1
          |  def seconds(pairs: Vector[(Page, Int)]) = pairs.map(second)
          |}""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.DisjointObject),
      Vector(
        "object Pairs holds 2 groups of members that never use each other and use different types " +
          "({first, firsts}, {second, seconds})"
      )
    )
  }

  test("string interpolation shared by groups of different domain types does not join them") {
    val found = findings(Seq(
      "Kinds.scala"  -> "case class Token(text: String)\ncase class Page(lines: Vector[String])",
      "Labels.scala" ->
        """object Labels {
          |  def label(token: Token): String = s"token ${token.text}"
          |  def labels(tokens: Vector[Token]) = tokens.map(label)
          |  def title(page: Page): String   = s"page ${page.lines.size}"
          |  def titles(pages: Vector[Page]) = pages.map(title)
          |}""".stripMargin
    ))
    assertEquals(
      of(found, Finding.Rule.DisjointObject),
      Vector(
        "object Labels holds 2 groups of members that never use each other and use different types " +
          "({label, labels}, {title, titles})"
      )
    )
  }

  test("a type spelled through the Predef alias and as its target is one type") {
    val found = findings(Seq(
      "Spellings.scala" ->
        """object Spellings {
          |  def same(text: String): String = text.take(1)
          |  def again(text: String): String = same(text).reverse
          |  def spelled(text: java.lang.String): java.lang.String = text.capitalize
          |  def respelled(text: java.lang.String): java.lang.String = spelled(text).reverse
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("constants and methods over String, Boolean and Char make no group of a nested object") {
    val found = findings(Seq(
      "Trace.scala" ->
        """class TraceState(val entries: Vector[(String, String)])
          |object TraceState {
          |  private object Validation {
          |    final val MaxEntries = 32
          |    private final val KeyMaxSize   = 256
          |    private final val ValueMaxSize = 256
          |    private final val VendorIdSize = 13
          |    def isKeyValid(key: String): Boolean = {
          |      @scala.annotation.tailrec
          |      def loop(idx: Int, multi: Boolean): (Boolean, Boolean) =
          |        if (idx < key.length) { if (key.charAt(idx) == '@') (false, multi) else loop(idx + 1, multi) }
          |        else (true, multi)
          |      key.length > 0 && key.length <= KeyMaxSize && loop(1, false)._1 && key.length - 1 < VendorIdSize
          |    }
          |    def isValueValid(value: String): Boolean =
          |      value.length > 0 && value.length <= ValueMaxSize && value.forall(char => char >= 0x20 && char != ',')
          |  }
          |  def entries(state: TraceState): Int = Validation.MaxEntries + state.entries.size
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("a group of Int constants beside a group of a domain class is not split") {
    val found = findings(Seq(
      "Pools.scala" ->
        """import java.nio.ByteBuffer
          |
          |object Sizes { val initSize = 1024; val expandSize = 4096 }
          |object BufferPool {
          |  private final val poolEntrySize1 = Sizes.expandSize + 16
          |  private final val maxBufferSize  = poolEntrySize1 * 2
          |  final class Pool { var misses = 0 }
          |  def allocate(pool: Pool, size: Int): Option[ByteBuffer] = { pool.misses += 1; Some(ByteBuffer.allocate(size)) }
          |  def allocateDirect(pool: Pool, size: Int): Option[ByteBuffer] = allocate(pool, size).map(identity)
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("a group that only calls library objects beside a group of domain types is not split") {
    val found = findings(Seq(
      "Kinds.scala"   -> "case class Span(content: String)\ncase class Block(spans: Vector[Span])",
      "Headers.scala" ->
        """object BlockNames {
          |  def encode(text: String)   = java.net.URLEncoder.encode(text.trim, "utf-8")
          |  def fragment(text: String) = "#" + encode(text)
          |  def textOf(spans: Seq[Span]) = spans.map(_.content).mkString
          |  def name(blocks: Seq[Block]) = blocks.headOption.map(block => textOf(block.spans)).getOrElse("Untitled")
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("groups over Set, Math and Double beside a group of a domain class are not split") {
    val found = findings(Seq(
      "Kinds.scala"    -> "case class Genome(values: Vector[Double])",
      "Breeding.scala" ->
        """object Breeding {
          |  val Primitives: Set[String] = Set("scala.Int", "scala.Long")
          |  def isPrimitive(name: String): Boolean = Primitives.contains(name)
          |  def log2(x: Int): Int = (math.log(x) / math.log(2)).toInt
          |  def logOf(size: Int): Int = math.max(log2(size) - 2, 1)
          |  def crossover(left: Genome, right: Genome): Genome = Genome(left.values.zip(right.values).map(Math.max(_, _)))
          |  def mutate(genome: Genome): Genome = crossover(genome, genome)
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("groups whose extension methods share a receiver type use a common type") {
    val found = findings(Seq(
      "Kinds.scala"  -> "class Bin[A](val value: A)\nclass Zippable[A]\nclass Unzippable[A]",
      "Tupled.scala" ->
        """opaque type Tupled[A] = Bin[A]
          |object Tupled {
          |  def atom[A](value: A): Tupled[A]  = Bin(value)
          |  def unzip[A](unzippable: Unzippable[A], value: A): Bin[A] = atom(value)
          |  extension [A](tupled: Tupled[A]) {
          |    def fold(zippable: Zippable[A]): A = tupled.value
          |    def foldWith(zippable: Zippable[A], other: A): A = fold(zippable)
          |  }
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("a group that matches on lists of characters beside a group of a library type is not split") {
    val found = findings(Seq(
      "Conversions.scala" ->
        """import scala.util.matching.Regex
          |
          |object Conversions {
          |  private val FirstPass = "([A-Z]+)([A-Z][a-z])".r
          |  def toSnake(name: String): String = FirstPass.replaceAllIn(name, "$1_$2").toLowerCase
          |  def toPascal(name: String): String = {
          |    def loop(x: List[Char]): List[Char] = (x: @unchecked) match {
          |      case '_' :: c :: rest => Character.toUpperCase(c) :: loop(rest)
          |      case '_' :: Nil       => Nil
          |      case c :: rest        => c :: loop(rest)
          |      case Nil              => Nil
          |    }
          |    loop('_' :: name.toList).mkString
          |  }
          |  def toCamel(name: String): String = toPascal(name).take(1).toLowerCase
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("a group over arrays of String beside a group of a domain type is not split") {
    val found = findings(Seq(
      "Kinds.scala" -> "case class Cpg(path: String)",
      "Tool.scala"  ->
        """object Tool {
          |  val Delimiter = "--frontend-args"
          |  def splitArgs(args: Array[String]): (List[String], List[String]) = {
          |    val index = args.indexOf(Delimiter)
          |    if (index == -1) (args.toList, Nil) else args.toList.splitAt(index)
          |  }
          |  def load(path: String): Cpg = Cpg(path)
          |  def reload(path: String): Cpg = load(path)
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("a member whose type is an alias of another member uses it") {
    val found = findings(Seq(
      "Kinds.scala" ->
        "trait Algebra\ntrait Svg\ntrait Module { type Alg <: Algebra; def instance: Alg }\ntrait Writer[A, F, S]\nclass FrameImpl(name: String)\nobject FrameImpl { def apply(name: String) = new FrameImpl(name) }",
      "Svgs.scala" ->
        """object Svgs {
          |  val jvm: Module                = new Module { type Alg = Algebra; def instance: Alg = new Algebra {} }
          |  type Alg                       = jvm.Alg
          |  val instance                   = jvm.instance
          |  type Frame                     = FrameImpl
          |  val Frame                      = FrameImpl
          |  implicit val writer: Writer[Alg, Frame, Svg] = new Writer[Alg, Frame, Svg] {}
          |  def frame: Frame               = Frame("dummy")
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("a member that calls a class nested in its object uses what the class uses") {
    val found = findings(Seq(
      "Kinds.scala" -> "trait Scribe\ntrait Clock",
      "Cats.scala"  ->
        """object cats {
          |  lazy val io: Scribe = apply(new Scribe {})
          |  def apply(scribe: Scribe): Scribe = scribe
          |  case class Timer(clock: Clock) { def log(label: String): Scribe = io }
          |  def timer(clock: Clock): Timer = Timer(clock)
          |  def timed(clock: Clock): Timer = timer(clock)
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }

  test("a type alone in its file that one type uses belongs beside it, as a tiny one two types use does") {
    val found = findings(
      Seq(
        "Small.scala"  -> "case class Small(value: Int)",
        "Big.scala"    -> "class Big { def small: Small = Small(1); def shared: Common = Common(1) }",
        "Common.scala" -> "case class Common(value: Int)",
        "Others.scala" -> "class First { def c: Common = Common(2) }\nclass Second { def c: Common = Common(3) }",
        "Wide.scala"   ->
          ("case class Wide(value: Int) {\n" + (1 to 10).map(n => s"  def f$n: Int = $n").mkString("\n") + "\n}"),
        "Uses.scala"  -> s"class Uses {\n  def wide: Wide = Wide(1)\n${body("more", 12)}\n}",
        "Fence.scala" -> "object Fence { def fence(): Int = 1 }",
        "Heap.scala"  -> "class Heap { def allocate: Int = Fence.fence() }"
      ),
      Seq("WideSuite.scala" -> "class WideSuite { def wide: Wide = Wide(2) }"),
      // One platform shares the source another replaces.
      twins = Seq(Seq("Fence.scala" -> "object Fence { def fence(): Int = 2 }"), Seq.empty)
    )
    assertEquals(
      of(found, Finding.Rule.SatelliteType),
      Vector(
        "Small is 1 line in a file of its own, used only by Big",
        "Wide is 12 lines in a file of its own, used only by Uses"
      )
    )
  }

  test("generated types are neither reported nor counted as satellites") {
    val found = findings(
      Seq(
        "src/generated/Tags.scala"  -> "object Tags { val all: Int = 1 }",
        "src/generated/Nodes.scala" -> "class Nodes(val nodes: Vector[Int])",
        "Big.scala"                 -> "class Big { def tags: Int = Tags.all; def nodes: Nodes = Nodes(Vector(1)) }"
      ),
      generated = GeneratedSources
    )
    assertEquals(of(found, Finding.Rule.SatelliteType), Vector.empty)
    assertEquals(of(found, Finding.Rule.FieldStutter), Vector.empty)
  }

  test("functions matching over an enum beside it are methods of the enum") {
    val found = findings(Seq(
      "Color.scala" ->
        """enum Color { case Red, Green, Blue }
          |object Palette {
          |  def label(color: Color): String = color match {
          |    case Color.Red   => "r"
          |    case Color.Green => "g"
          |    case Color.Blue  => "b"
          |  }
          |  def warm(color: Color): Boolean = color match {
          |    case Color.Red => true
          |    case _         => false
          |  }
          |}""".stripMargin,
      "Shade.scala" ->
        """enum Shade { case Light, Dark }
          |object Shades {
          |  def code(shade: Shade): Int = shade match { case Shade.Light => 0; case Shade.Dark => 1 }
          |  def name(shade: Shade): String = shade match { case Shade.Light => "l"; case Shade.Dark => "d" }
          |  val offset: Int                = 1
          |  def shifted(shade: Shade): Int = shade match { case Shade.Light => offset; case Shade.Dark => offset + 1 }
          |}
          |class Stage {
          |  private enum Phase { case Early, Late }
          |  def step: Int                    = order(Phase.Early)
          |  private def order(phase: Phase): Int = phase match { case Phase.Early => 0; case Phase.Late => 1 }
          |}""".stripMargin,
      "Render.scala" ->
        """object Render {
          |  def show(color: Color): String = color match { case Color.Red => "R"; case _ => "?" }
          |  def full(color: Color): Int    = color match { case Color.Red => 1; case Color.Green => 2; case Color.Blue => 3 }
          |}""".stripMargin
    ))
    val dispatched = found.filter(_.rule == Finding.Rule.EnumDispatchFunction)
    assertEquals(
      dispatched.map(finding => (finding.level, finding.detail.takeWhile(_ != ';'))).sortBy(_._2),
      Vector(
        Finding.Level.Warning -> "code matches its Shade parameter shade over its cases",
        Finding.Level.Info    -> "label matches its Color parameter color over its cases",
        Finding.Level.Warning -> "name matches its Shade parameter shade over its cases",
        Finding.Level.Info    -> "order matches its Phase parameter phase over its cases"
      )
    )
  }

  private def satellites(found: Vector[Finding]): Vector[String] =
    found.collect { case finding if finding.rule == Finding.Rule.SatelliteType => finding.detail }.sorted

  private def body(name: String, lines: Int): String = (1 to lines).map(n => s"  def $name$n: Int = $n").mkString("\n")

  test("a type tied by its signatures to another belongs with it, whichever type calls it") {
    val found = findings(Seq(
      "Tidy.scala" ->
        s"""object Tidy {
           |  case class Plan(moves: Vector[String])
           |  case class Op(name: String)
           |  def scan(root: String): Vector[String] = Vector(root)
           |${body("extra", 10)}
           |}""".stripMargin,
      "AviTidy.scala" ->
        s"""object AviTidy {
           |  def plan(root: String): Tidy.Plan = Tidy.Plan(Tidy.scan(root))
           |  def op(name: String): Tidy.Op = Tidy.Op(name)
           |${body("more", 10)}
           |}""".stripMargin,
      "StripPrefix.scala" ->
        s"""object StripPrefix {
           |  def plan(dir: String): Tidy.Plan = Tidy.Plan(Vector(dir))
           |${body("more", 10)}
           |}""".stripMargin,
      "Commands.scala" ->
        """class Commands {
          |  def avi: Int = AviTidy.plan("a").moves.size
          |  def strip: Int = StripPrefix.plan("b").moves.size
          |  def tidy: Int = Tidy.scan("c").size
          |}""".stripMargin
    ))
    assertEquals(
      satellites(found),
      Vector(
        "AviTidy is 14 lines in a file of its own, tied to Tidy by its signatures; nest it in the companion of Tidy",
        "StripPrefix is 13 lines in a file of its own, tied to Tidy by its signatures; nest it in the companion of Tidy"
      )
    )
  }

  test("a type extending the nested trait of one type is tied to it, and not to the types it merely holds") {
    val found = findings(Seq(
      "Scheduler.scala" ->
        s"""object Scheduler {
           |  case class Outcome(ok: Boolean)
           |  trait Listener { def done(outcome: Outcome): Unit }
           |${body("extra", 10)}
           |}""".stripMargin,
      "Progress.scala"    -> s"class Progress {\n  def finished(): Unit = ()\n${body("more", 10)}\n}",
      "JobReporter.scala" ->
        s"""class JobReporter(progress: Progress) extends Scheduler.Listener {
           |  def done(outcome: Scheduler.Outcome): Unit = progress.finished()
           |${body("more", 10)}
           |}""".stripMargin,
      "Routes.scala" ->
        s"""object Routes {
           |  def show(progress: Progress): Int = progress.hashCode
           |${body("more", 10)}
           |}""".stripMargin,
      "Runner.scala" ->
        """class Runner {
          |  def reporter(progress: Progress): Int = JobReporter(progress).hashCode + Routes.show(progress)
          |  def listen(listener: Scheduler.Listener): Unit = listener.done(Scheduler.Outcome(true))
          |}""".stripMargin,
      "Video.scala" ->
        "class Video { def reporter(progress: Progress): Int = JobReporter(progress).hashCode + Routes.more1 }"
    ))
    assertEquals(
      satellites(found).filter(finding => finding.startsWith("JobReporter") || finding.startsWith("Routes")),
      Vector(
        "JobReporter is 13 lines in a file of its own, tied to Scheduler by its signatures; " +
          "nest it in the companion of Scheduler"
      )
    )
  }

  test("a type that only takes the nested types of another as settings belongs with the type that calls it") {
    val found = findings(Seq(
      "Http.scala"   -> s"object Http {\n  class Retrying(val attempts: Int)\n${body("more", 40)}\n}",
      "Discuz.scala" ->
        s"object Discuz {\n  def fetch(retrying: Http.Retrying): Int = retrying.attempts\n${body("more", 20)}\n}",
      "Dm.scala" ->
        s"""class Dm {
           |  def go: Int = Discuz.fetch(Http.Retrying(1)) + Http.more1
           |${body("more", 40)}
           |}""".stripMargin
    ))
    assertEquals(
      satellites(found).filter(_.startsWith("Discuz")),
      Vector("Discuz is 23 lines in a file of its own, used only by Dm; nest it in the companion of Dm")
    )
  }

  test("a type that only one other names in a signature belongs with it, whoever else calls it") {
    val found = findings(Seq(
      "VNDB.scala" ->
        s"""class VNDB(url: String) {
           |  def lookup(id: String): VNDB.Entry = VNDB.Entry(id)
           |${body("more", 10)}
           |}
           |object VNDB {
           |  case class Entry(title: String)
           |  val DefaultUrl: String = "u"
           |}""".stripMargin,
      "VNCG.scala" -> s"""object VNCG {
           |  def name(entry: VNDB.Entry): String = entry.title
           |  def fetch(vndb: VNDB): String = vndb.lookup("a").title
           |${body("more", 20)}
           |}""".stripMargin,
      "Commands.scala" ->
        """class Commands {
          |  def named: String = VNCG.name(VNDB(VNDB.DefaultUrl).lookup("a"))
          |}""".stripMargin
    ))
    assertEquals(
      satellites(found).filter(finding => finding.startsWith("VNDB") || finding.startsWith("VNCG")),
      Vector(
        "VNDB is 17 lines in a file of its own, named in a signature only by VNCG; nest it in the companion of VNCG"
      )
    )
  }

  test("a type far larger than its host, or one of three a host would take, is offered no home") {
    val found = findings(Seq(
      "Large.scala"  -> s"object Large {\n${body("more", 60)}\n}",
      "Holder.scala" -> "class Holder { def go: Int = Large.more1 }",
      "Hub.scala"    ->
        s"""class Hub {
           |  def first: Int = First.more1
           |  def second: Int = Second.more1
           |  def third: Int = Third.more1
           |${body("more", 30)}
           |}""".stripMargin,
      "First.scala"  -> s"object First {\n${body("more", 8)}\n}",
      "Second.scala" -> s"object Second {\n${body("more", 8)}\n}",
      "Third.scala"  -> s"object Third {\n${body("more", 8)}\n}"
    ))
    assertEquals(satellites(found), Vector.empty)
  }

  test("an entry point that only runs one type belongs in the companion of that type") {
    val found = findings(Seq(
      "Commands.scala" -> "case class Commands(out: String) {\n  def run(args: Vector[String]): Int = args.size\n}",
      "Other.scala"    -> "case class Other(out: String) {\n  def run(args: Vector[String]): Int = args.size\n}",
      "Cli.scala"      ->
        """object Cli {
          |  def run(args: Vector[String], out: String): Int = Commands(out).run(args)
          |  def main(args: Array[String]): Unit = sys.exit(run(args.toVector, "o"))
          |}""".stripMargin,
      "Tool.scala" ->
        """object Tool {
          |  def main(args: Array[String]): Unit = Commands("t").run(Vector.empty)
          |  def helper: Int = 3
          |}""".stripMargin,
      "Pair.scala" ->
        """object Pair {
          |  def main(args: Array[String]): Unit = {
          |    Commands("a").run(Vector.empty)
          |    Other("b").run(Vector.empty)
          |  }
          |}""".stripMargin
    ))
    assertEquals(
      satellites(found).filter(_.startsWith("Cli")),
      Vector("Cli is an entry point of 4 lines that only runs Commands; nest it in the companion of Commands")
    )
    assertEquals(
      satellites(found).filter(finding => finding.startsWith("Tool") || finding.startsWith("Pair")),
      Vector.empty
    )
  }

  test("a helper several types use, or one that tests use alone, is no satellite") {
    val found = findings(
      Seq(
        "Locks.scala" ->
          """import java.util.concurrent.locks.ReentrantLock
             |object Locks {
             |  extension (lock: ReentrantLock) {
             |    inline def locked[A](inline body: => A): A = {
             |      lock.lock()
             |      try body
             |      finally lock.unlock()
             |    }
             |  }
             |}""".stripMargin,
        "Stamp.scala" ->
          s"case class Stamp(size: Long) {\n  def same(other: Stamp): Boolean = size == other.size\n${body("more", 10)}\n}",
        "Marker.scala"    -> s"object Marker {\n  val Name: String = \"m\"\n${body("more", 10)}\n}",
        "Alone.scala"     -> s"object Alone {\n${body("more", 12)}\n}",
        "Operation.scala" ->
          """import java.util.concurrent.locks.ReentrantLock
            |class Operation { def go(lock: ReentrantLock): Long = Locks.locked(lock)(Stamp(1).size) }""".stripMargin,
        "Check.scala" ->
          """import java.util.concurrent.locks.ReentrantLock
            |class Check { def go(lock: ReentrantLock): Boolean = Locks.locked(lock)(Stamp(1).same(Stamp(2))) }""".stripMargin,
        "Recode.scala" ->
          """import java.util.concurrent.locks.ReentrantLock
            |class Recode { def go(lock: ReentrantLock): String = Locks.locked(lock)(Marker.Name) }""".stripMargin,
        "Scan.scala" -> "class Scan { def go: String = Marker.Name; def again: String = Marker.Name + Marker.Name }",
        "Tag.scala"  -> "class Tag { def go: String = Marker.Name }"
      ),
      Seq("AloneSuite.scala" -> "class AloneSuite { def go: Int = Alone.more1 }")
    )
    assertEquals(
      satellites(found).filter(finding => Vector("Locks", "Stamp", "Marker", "Alone").exists(finding.startsWith(_))),
      Vector.empty
    )
  }

  test("a type of a library's exported API is no satellite, however few types use it") {
    val found = findings(
      Seq(
        "Zip.scala"    -> s"object Zip {\n${body("more", 20)}\n}",
        "Recode.scala" -> "class Recode { def go: Int = Zip.more1 }"
      ),
      libraries = Set("m")
    )
    assertEquals(satellites(found), Vector.empty)
  }

  test("a group over repeated String parameters and wrapped numbers beside a group of a domain type is not split") {
    val found = findings(Seq(
      "Kinds.scala"   -> "case class Pixel(x: Int)",
      "Windows.scala" ->
        """object Windows {
          |  def draw(pixel: Pixel): Pixel = repaint(pixel)
          |  def repaint(pixel: Pixel): Pixel = Pixel(pixel.x + 1)
          |  def isOS(name: String): Boolean = name.toLowerCase.startsWith("win") && isInProc("windows", "wsl")
          |  @inline private def isInProc(parts: String*): Boolean =
          |    parts.map(_.toLowerCase).exists(part => part.nonEmpty && 3.max(part.length) > 2 && BigInt(1) < BigInt(2))
          |}""".stripMargin
    ))
    assertEquals(of(found, Finding.Rule.DisjointObject), Vector.empty)
  }
}
