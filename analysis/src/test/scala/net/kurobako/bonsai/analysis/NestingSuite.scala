package net.kurobako.bonsai.analysis

/** The patterns the nesting rule learns, as small sources of the cases that gave them. Each accepted case also has the
  * source with the suggestion applied, which must give no suggestion, and each rejected case is already steady.
  */
class NestingSuite extends AnalysisSuite {
  private def moves(
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      config: Nesting.Config = Nesting.Config()
  ): Vector[String] = {
    val result = extract(main, test, libraries = Set("m"))
    Nesting.plan(result.nesting, config).reported.map(describe).sorted
  }

  private def describe(move: Nesting.Move): String = {
    val name = move.declaration.name.stripPrefix("demo.")
    (move.from.owner, move.to.owner) match {
      case (from, Some(to)) if from.contains(to) => s"$name -> companion of ${to.stripPrefix("demo.")}"
      case (_, Some(to))                         => s"$name -> ${to.stripPrefix("demo.")}"
      case (_, None)                             => s"$name -> top"
    }
  }

  private def file(name: String, source: String): (String, String) =
    s"demo/$name.scala" -> s"package demo\n${source.stripMargin}"

  private def steady(
      before: Seq[(String, String)],
      expected: Vector[String],
      after: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      moved: Seq[(String, String)] = Nil,
      config: Nesting.Config = Nesting.Config()
  ): Unit = {
    assertEquals(moves(before, test, config), expected)
    assertEquals(moves(after, if (moved.isEmpty) test else moved, config), Vector.empty[String])
  }

  test("a constant only a nested class reads moves into that class") {
    steady(
      Seq(file(
        "Metadata",
        """object CodeObjectMetadata {
          |  private val MetadataNoteType = 32
          |  private val MaximumDepth = 64
          |  def read(bytes: Array[Byte]): Int = MetadataNoteType + new Reader(bytes).skip()
          |  private class Reader(bytes: Array[Byte]) {
          |    def skip(): Int = bytes.length + MaximumDepth
          |  }
          |}"""
      )),
      Vector("CodeObjectMetadata.MaximumDepth -> CodeObjectMetadata.Reader"),
      Seq(file(
        "Metadata",
        """object CodeObjectMetadata {
          |  private val MetadataNoteType = 32
          |  def read(bytes: Array[Byte]): Int = MetadataNoteType + new Reader(bytes).skip()
          |  private class Reader(bytes: Array[Byte]) {
          |    def skip(): Int = bytes.length + Reader.MaximumDepth
          |  }
          |  private object Reader {
          |    private val MaximumDepth = 64
          |  }
          |}"""
      ))
    )
  }

  test("a nested class that needs no instance of its class moves to the companion") {
    steady(
      Seq(file(
        "LoadedProgram",
        """object LoadedProgram { def load(): LoadedProgram = new LoadedProgram }
          |class LoadedProgram {
          |  private case class Bound(value: Int)
          |  private val bound = collection.mutable.Map.empty[String, Bound]
          |  def bind(name: String): Int = { bound(name) = Bound(1); bound.size }
          |}"""
      )),
      Vector("LoadedProgram.Bound -> companion of LoadedProgram"),
      Seq(file(
        "LoadedProgram",
        """object LoadedProgram {
          |  def load(): LoadedProgram = new LoadedProgram
          |  private case class Bound(value: Int)
          |}
          |class LoadedProgram {
          |  import LoadedProgram.Bound
          |  private val bound = collection.mutable.Map.empty[String, Bound]
          |  def bind(name: String): Int = { bound(name) = Bound(1); bound.size }
          |}"""
      ))
    )
  }

  test("a class that uses the instance of its class stays inside it") {
    assertEquals(
      moves(Seq(file(
        "Counter",
        """class Counter {
          |  private var count = 0
          |  private class Step { def next(): Int = { count += 1; count } }
          |  def run(): Int = new Step().next()
          |}"""
      ))),
      Vector.empty[String]
    )
  }

  test("a nested class of a class without a companion gets one") {
    steady(
      Seq(file(
        "ResourceRender",
        """class Renderer { def run(): Int = new ResourceRender().stored(1) }
          |class Printer { def run(): Int = new ResourceRender().stored(2) }
          |private class ResourceRender {
          |  private case class Span(first: Int, packing: Int)
          |  def stored(first: Int): Int = Span(first, 2).first
          |}"""
      )),
      Vector("ResourceRender.Span -> companion of ResourceRender"),
      Seq(file(
        "ResourceRender",
        """class Renderer { def run(): Int = new ResourceRender().stored(1) }
          |class Printer { def run(): Int = new ResourceRender().stored(2) }
          |private object ResourceRender {
          |  private case class Span(first: Int, packing: Int)
          |}
          |private class ResourceRender {
          |  import ResourceRender.Span
          |  def stored(first: Int): Int = Span(first, 2).first
          |}"""
      ))
    )
  }

  private val renderContext = file(
    "RenderContext",
    """private object RenderContext {
      |  @FunctionalInterface
      |  trait IndexedTerm { def apply(index: Int): Int }
      |  case class Exchange(address: Int, bank: Int)
      |}"""
  )

  test("a function-like trait and a type that would make two files use each other stay where they are") {
    val lane = file(
      "LaneRenderer",
      """class LaneRenderer {
        |  def run(): Int = new ResourceRender().exchanged(RenderContext.Exchange(1, 2)) + matrix((index: Int) => index)
        |  private def matrix(registers: RenderContext.IndexedTerm): Int = registers(1)
        |}"""
    )
    val resource = file(
      "ResourceRender",
      """private class ResourceRender {
        |  def exchanged(exchange: RenderContext.Exchange): Int = exchange.address + exchange.bank
        |  def located(valid: RenderContext.IndexedTerm): Int = valid(3)
        |  def twice(valid: RenderContext.IndexedTerm): Int = located(valid) + located(valid)
        |}"""
    )
    assertEquals(moves(Seq(renderContext, lane, resource), config = Nesting.Config(size = 4)), Vector.empty[String])
    assertEquals(
      moves(Seq(renderContext, lane, resource)),
      Vector("RenderContext.Exchange -> LaneRenderer", "ResourceRender -> LaneRenderer")
    )
  }

  test("a type only a sibling reaches moves into the companion of that sibling when both stay in one file") {
    steady(
      Seq(file(
        "RenderContext",
        """private object RenderContext {
          |  trait Layout { def width: Int }
          |  case class Spelling(text: String)
          |  private class Spelled(spellings: String => Option[String]) extends Naming[Spelling] {
          |    def resolve(spelling: Spelling): Int = spellings(spelling.text).size
          |  }
          |  class Names(spellings: String => Option[String]) extends Scope[Spelling](Spelled(spellings)) with Layout {
          |    def width: Int = 1
          |    def named(text: String): Int = operands(Spelling(text)) + width
          |  }
          |}
          |trait Naming[-O] { def resolve(operand: O): Int }
          |class Scope[O](naming: Naming[O]) { def operands(operand: O): Int = naming.resolve(operand) }
          |class LaneRenderer { def run(names: RenderContext.Names): Int = names.named("a") + (names: RenderContext.Layout).width }
          |class ResourceRender { def run(names: RenderContext.Names): Int = names.named("b") }"""
      )),
      Vector("RenderContext.Spelled -> RenderContext.Names", "RenderContext.Spelling -> RenderContext.Names"),
      Seq(file(
        "RenderContext",
        """private object RenderContext {
          |  trait Layout { def width: Int }
          |  class Names(spellings: String => Option[String]) extends Scope[Names.Spelling](Names.Spelled(spellings)) with Layout {
          |    def width: Int = 1
          |    def named(text: String): Int = operands(Names.Spelling(text)) + width
          |  }
          |  object Names {
          |    case class Spelling(text: String)
          |    private class Spelled(spellings: String => Option[String]) extends Naming[Spelling] {
          |      def resolve(spelling: Spelling): Int = spellings(spelling.text).size
          |    }
          |  }
          |}
          |trait Naming[-O] { def resolve(operand: O): Int }
          |class Scope[O](naming: Naming[O]) { def operands(operand: O): Int = naming.resolve(operand) }
          |class LaneRenderer { def run(names: RenderContext.Names): Int = names.named("a") + (names: RenderContext.Layout).width }
          |class ResourceRender { def run(names: RenderContext.Names): Int = names.named("b") }"""
      ))
    )
  }

  private val cache = file(
    "LoadedProgram",
    """class LoadedProgram {
      |  private val cache = Vector(CodeObjectKey(Vector(1)))
      |  def size: Int = cache.size
      |}"""
  )

  test("a small private type only one class uses moves into the companion of that class, whatever the tests use") {
    val key = file(
      "CodeObjectKey",
      """private case class CodeObjectKey(image: Vector[Int]) {
        |  private val hash = image.sum
        |  override def hashCode(): Int = hash
        |}"""
    )
    val tests =
      Seq(file("CodeObjectIndexSuite", "object CodeObjectIndexSuite { val first = CodeObjectKey(Vector(1)) }"))
    steady(
      Seq(cache, key),
      Vector("CodeObjectKey -> LoadedProgram"),
      Seq(file(
        "LoadedProgram",
        """object LoadedProgram {
          |  private[demo] case class CodeObjectKey(image: Vector[Int]) {
          |    private val hash = image.sum
          |    override def hashCode(): Int = hash
          |  }
          |}
          |class LoadedProgram {
          |  import LoadedProgram.CodeObjectKey
          |  private val cache = Vector(CodeObjectKey(Vector(1)))
          |  def size: Int = cache.size
          |}"""
      )),
      tests,
      Seq(file(
        "CodeObjectIndexSuite",
        "import LoadedProgram.CodeObjectKey\nobject CodeObjectIndexSuite { val first = CodeObjectKey(Vector(1)) }"
      ))
    )
  }

  test("the size of the owner does not matter, the size of what moves does") {
    val key = file("CodeObjectKey", "private case class CodeObjectKey(image: Vector[Int])")
    val big = file(
      "LoadedProgram",
      """class LoadedProgram {
        |  private val cache = Vector(CodeObjectKey(Vector(1)))
        |  def a: Int = cache.size
        |  def b: Int = cache.size
        |  def c: Int = cache.size
        |  def d: Int = cache.size
        |}"""
    )
    assertEquals(moves(Seq(big, key), config = Nesting.Config(size = 2)), Vector("CodeObjectKey -> LoadedProgram"))
    assertEquals(
      moves(
        Seq(file("CodeObjectKey", "private case class CodeObjectKey(\n  image: Vector[Int]\n)\n"), big),
        config = Nesting.Config(size = 2)
      ),
      Vector.empty[String]
    )
  }

  test("a type that reaches the owner only through a nested class lands in a file with the owner at depth one") {
    val state = file(
      "VMState",
      """private object VMState {
        |  trait Sized { def count: Int }
        |  class LaneBits(lanes: Int) { def count: Int = lanes }
        |  class Registers(lanes: Int) extends Sized { def count: Int = lanes + 1 }
        |}"""
    )
    val interpreter = file(
      "VMInterpreter",
      """object VMInterpreter {
        |  private class WaveRun {
        |    private val carry = VMState.LaneBits(1)
        |    def run(): Int = carry.count + VMState.Registers(2).count
        |  }
        |  def run(): Int = new WaveRun().run()
        |}
        |class Other { def run(): Int = VMState.Registers(3).count }"""
    )
    assertEquals(moves(Seq(state, interpreter)), Vector.empty[String])
    assertEquals(
      moves(Seq(state, interpreter), config = Nesting.Config(depth = 3)),
      Vector("VMState.LaneBits -> VMInterpreter.WaveRun")
    )
  }

  private val naming = file(
    "Naming",
    """trait Naming[-O] { def resolve(operand: O): Vector[Naming.Name] }
      |object Naming {
      |  case class Name(name: String, part: Int)
      |  object Name { def spelt(name: String): Name = Name(name, 0) }
      |}"""
  )

  test(
    "a nested type that uses nothing of its parent and has users that use nothing else of it moves to the top level"
  ) {
    val scope = file(
      "Scope",
      """class Scope[O](naming: Naming[O]) {
        |  def declare(name: Naming.Name): Int = naming.resolve(null.asInstanceOf[O]).size + name.part
        |}"""
    )
    val renderer = file("Renderer", "class Renderer { def run(): Int = Naming.Name.spelt(\"a\").part }")
    steady(
      Seq(naming, scope, renderer),
      Vector("Naming.Name -> top"),
      Seq(
        file(
          "Naming",
          """trait Naming[-O] { def resolve(operand: O): Vector[Name] }
            |case class Name(name: String, part: Int)
            |object Name { def spelt(name: String): Name = Name(name, 0) }"""
        ),
        file(
          "Scope",
          """class Scope[O](naming: Naming[O]) {
            |  def declare(name: Name): Int = naming.resolve(null.asInstanceOf[O]).size + name.part
            |}"""
        ),
        file("Renderer", "class Renderer { def run(): Int = Name.spelt(\"a\").part }")
      )
    )
  }

  test("a nested type that uses its parent stays, however many use it") {
    assertEquals(
      moves(Seq(
        file(
          "Term",
          """object Term {
            |  trait Part { def size: Int }
            |  case class Step(name: String) extends Part { def size: Int = name.size }
            |  case class Steps(all: Vector[Step]) extends Part { def size: Int = all.size; def last: Step = all.last }
            |}"""
        ),
        file("Lane", "class Lane { def run(steps: Term.Steps): Int = steps.all.size }"),
        file("Stage", "class Stage { def run(steps: Term.Steps): Int = steps.last.name.size }")
      )),
      Vector.empty[String]
    )
  }

  test("a namespace that only one object uses moves into it, with what it holds") {
    steady(
      Seq(
        file(
          "PrimitiveLayout",
          """private[demo] object PrimitiveLayout {
            |  enum Argument { case Value, Result }
            |  case class Layout(owner: String, arguments: Vector[Argument])
            |  def term(layout: Layout, operand: Int): Int = layout.arguments.size + operand
            |  def of(owner: String): Layout = Layout(owner, Vector(Argument.Value))
            |}"""
        ),
        file(
          "Render",
          "object Render { def primitive(operand: Int): Int = PrimitiveLayout.term(PrimitiveLayout.of(\"a\"), operand) }"
        )
      ),
      Vector("PrimitiveLayout -> Render"),
      Seq(file(
        "Render",
        """object Render {
          |  private object PrimitiveLayout {
          |    enum Argument { case Value, Result }
          |    case class Layout(owner: String, arguments: Vector[Argument])
          |    def term(layout: Layout, operand: Int): Int = layout.arguments.size + operand
          |    def of(owner: String): Layout = Layout(owner, Vector(Argument.Value))
          |  }
          |  def primitive(operand: Int): Int = PrimitiveLayout.term(PrimitiveLayout.of("a"), operand)
          |}"""
      ))
    )
  }

  test("a trait with one implementer stays, as a seam and not a detail") {
    assertEquals(
      moves(Seq(
        file("Backend", "trait Backend { def show(definition: Int): String }"),
        file(
          "ScalaBackend",
          "object ScalaBackend extends Backend { def show(definition: Int): String = definition.toString }"
        )
      )),
      Vector.empty[String]
    )
  }

  test("opaque types, their companions and extensions never move") {
    assertEquals(
      moves(Seq(
        file(
          "prelude",
          """object prelude {
            |  opaque type BFloat16 = Long
            |  object BFloat16 { inline def apply(bits: Long): BFloat16 = bits }
            |  extension (value: BFloat16) { def bits: Long = value }
            |}"""
        ),
        file(
          "AtomicFloats",
          """import prelude.*
            |object AtomicFloats { def sum(old: BFloat16): BFloat16 = BFloat16(old.bits + 1) }"""
        )
      )),
      Vector.empty[String]
    )
  }

  private val typer = file(
    "Typer",
    """private[demo] object Typer {
      |  private def typed(value: Int): Int = value + 1
      |  def settle(value: Int): Int = typed(value) * 2
      |  def typedTerm(value: Int): Int = typed(value)
      |  def condition(value: Int): Int = typed(value) - 1
      |}"""
  )

  test("a unit too large to be a detail stays whole, and so do the helpers that would move with it") {
    val definition = file(
      "Definition",
      """object Definition {
        |  def read(value: Int): Int = Typer.typedTerm(value) + Typer.settle(value)
        |  def check(value: Int): Int = Typer.condition(value)
        |}"""
    )
    assertEquals(moves(Seq(typer, definition), config = Nesting.Config(size = 3)), Vector.empty[String])
    assertEquals(
      moves(Seq(typer, definition)),
      Vector("Typer -> Definition")
    )
  }

  private val floats = file(
    "Floats",
    """private[demo] object Floats {
      |  def cvtPkrtz(low: Int, high: Int): Int = low | high
      |  def sqrt(value: Int): Int = helper(value) * 2
      |  def helper(value: Int): Int = value + 1
      |}"""
  )

  test("a single-user helper moves to the object that calls it, and one that needs its siblings does not") {
    val semantics = file(
      "FloatSemantics",
      """private object FloatSemantics {
        |  def convert(value: Int): Int = Floats.cvtPkrtz(value, value)
        |  def root(value: Int): Int = Floats.sqrt(value)
        |}"""
    )
    val users = Seq(
      file(
        "Lane",
        "class Lane { def run(): Int = FloatSemantics.convert(1) + FloatSemantics.root(2) + Floats.helper(3) }"
      ),
      file("Stage", "class Stage { def run(): Int = FloatSemantics.convert(4) + FloatSemantics.root(5) }")
    )
    steady(
      Seq(floats, semantics) ++ users,
      Vector("Floats.cvtPkrtz -> FloatSemantics"),
      Seq(
        file(
          "Floats",
          """private[demo] object Floats {
            |  def sqrt(value: Int): Int = helper(value) * 2
            |  def helper(value: Int): Int = value + 1
            |}"""
        ),
        file(
          "FloatSemantics",
          """private object FloatSemantics {
            |  def cvtPkrtz(low: Int, high: Int): Int = low | high
            |  def convert(value: Int): Int = cvtPkrtz(value, value)
            |  def root(value: Int): Int = Floats.sqrt(value)
            |}"""
        )
      ) ++ users
    )
  }

  test("overloads move one by one, and tests that use them do not hold them back") {
    val widen = file(
      "MatrixArithmetic",
      """private[demo] object MatrixArithmetic {
        |  def widen(value: Int): Long = value.toLong
        |  def widen(value: Short): Long = value.toLong
        |  def round(value: Int): Int = value
        |}"""
    )
    val semantics = file(
      "MatrixSemantics",
      """private object MatrixSemantics {
        |  def first(value: Int): Long = MatrixArithmetic.widen(value)
        |  def second(value: Short): Long = MatrixArithmetic.widen(value)
        |}"""
    )
    val users = Seq(
      file(
        "Lane",
        "class Lane { def run(): Long = MatrixSemantics.first(1) + MatrixSemantics.second(2) + MatrixArithmetic.round(1) }"
      ),
      file(
        "Stage",
        "class Stage { def run(): Long = MatrixSemantics.first(1) + MatrixSemantics.second(2) + MatrixArithmetic.round(2) }"
      )
    )
    val tests = Seq(file("MatrixSuite", "object MatrixSuite { val widened = MatrixArithmetic.widen(1) }"))
    assertEquals(
      moves(Seq(widen, semantics) ++ users, tests),
      Vector("MatrixArithmetic.widen(Int) -> MatrixSemantics", "MatrixArithmetic.widen(Short) -> MatrixSemantics")
    )
  }

  test("a type its dominator reaches only through a file that depends on that dominator stays") {
    val compiler = file(
      "VMCompiler",
      """object VMCompiler {
        |  def compile(machine: Int): Int =
        |    new VMResourceDecoder().decode(machine) + DecodedRegisters.vector(machine) + DecodedRegisters.scalar(machine)
        |}"""
    )
    val decoder = file(
      "VMResourceDecoder",
      """private class VMResourceDecoder {
        |  def decode(machine: Int): Int = DecodedRegisters.scalar(machine) + DecodedRegisters.vector(machine) + 1
        |  def second(machine: Int): Int = machine + 2
        |  def third(machine: Int): Int = machine + 3
        |}"""
    )
    val registers = file(
      "DecodedRegisters",
      """private object DecodedRegisters {
        |  def vector(machine: Int): Int = machine
        |  def scalar(machine: Int): Int = machine + 1
        |}"""
    )
    assertEquals(moves(Seq(compiler, decoder, registers), config = Nesting.Config(size = 4)), Vector.empty[String])
  }

  test("a declaration that suppresses the rule, or sits in one that does, stays") {
    val key = file(
      "CodeObjectKey",
      """@SuppressWarnings(Array("bonsai:nesting"))
        |private case class CodeObjectKey(image: Vector[Int])"""
    )
    assertEquals(moves(Seq(cache, key)), Vector.empty[String])
    assertEquals(
      moves(Seq(
        file(
          "LoadedProgram",
          """@SuppressWarnings(Array("bonsai:nesting"))
            |class LoadedProgram {
            |  private case class Bound(value: Int)
            |  def bind(): Int = Bound(1).value
            |}"""
        )
      )),
      Vector.empty[String]
    )
  }

  test("the depth is the cap of the target, a damping of two wants two users") {
    val names = Seq(
      file(
        "Names",
        """class Names { def run(): Int = Names.make() }
          |object Names {
          |  def make(): Int = new Spelled().size
          |  private class Spelled { def size: Int = Mark(1).value }
          |  private case class Mark(value: Int)
          |}"""
      )
    )
    assertEquals(moves(names), Vector("Names.Mark -> Names.Spelled"))
    assertEquals(moves(names, config = Nesting.Config(depth = 1)), Vector.empty[String])
    assertEquals(moves(names, config = Nesting.Config(damping = 2)), Vector.empty[String])
  }
}
