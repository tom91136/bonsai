package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.analysis.ClassFiles.*
import net.kurobako.bonsai.testing.*
import org.objectweb.asm.Opcodes
import scala.util.Using

class CallGraphSuite extends AnalysisSuite {
  private val mark = """package fixture;

public class Mark {
  public static int circle() { return 1; }
  public static int square() { return 2; }
  public static int dead() { return 3; }
  public static int hello() { return 4; }
  public static int over() { return 5; }
  public static int ran() { return 6; }
  public static int ranDead() { return 7; }
  public static int stat() { return 8; }
  public static int marked() { return 9; }
  public static int bridged() { return 10; }
  public static int base() { return 11; }
  public static int sub() { return 12; }
  public static int field() { return 13; }
  public static int defaulted() { return 14; }
  public static int plain() { return 15; }
  public static int work() { return 16; }
  public static int threaded() { return 17; }
  public static int boxed() { return 18; }
  public static int named() { return 19; }
  public static int bound() { return 20; }
  public static int virt() { return 21; }
  public static int hook() { return 22; }
}
"""

  private val base = """package fixture;

public abstract class JBase {
  public abstract int go();

  public int base() {
    return Mark.base();
  }
}
"""

  private val provider = """package fixture;

public class Provider implements Runnable {
  public Provider() {}

  Provider(int hidden) {}

  public void run() {
    Mark.ran();
  }

  public void extra() {
    Mark.dead();
  }

  private void secret() {
    Mark.dead();
  }

  public static Provider provider() {
    return new Provider();
  }
}
"""

  private val api = """package fixture;

public class Api {
  public int open() { return Mark.circle(); }
  protected int guarded() { return Mark.square(); }
  int local() { return Mark.dead(); }
  private int hidden() { return Mark.dead(); }
}
"""

  private val shapes = """package fixture;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

interface Shape {
  int area();
}

class Circle implements Shape {
  public int area() { return Mark.circle(); }
}

class Square implements Shape {
  public int area() { return Mark.square(); }
}

class Orders {
  static Shape make() { return new Square(); }

  static int call(Shape shape) { return shape.area(); }

  static int siteThenNew(Shape shape) {
    call(shape);
    make();
    return 0;
  }

  static int newThenSite(Shape shape) {
    make();
    call(shape);
    return 0;
  }

  static int circles() { return call(new Circle()); }
}

class Live {
  int used() { return 1; }

  int unused() { return Mark.dead(); }
}

class Dead {
  static int run() { return new Live().used(); }
}

interface Greeter {
  default int greet() { return Mark.hello(); }

  int other();
}

class Impl implements Greeter {
  public int other() { return 1; }
}

class Over implements Greeter {
  public int other() { return 2; }

  public int greet() { return Mark.over(); }
}

class Defaults {
  static int run(Greeter greeter) { return greeter.greet(); }

  static int entry() { return run(new Impl()); }
}

class Lambdas {
  static int invoke() {
    Runnable run = () -> Mark.ran();
    run.run();
    return 0;
  }

  static int twice() {
    Runnable first = () -> Mark.ran();
    Runnable second = () -> Mark.stat();
    first.run();
    return 0;
  }

  static Runnable unreachable() { return () -> Mark.ranDead(); }

  static int callOnly(Runnable run) {
    run.run();
    return 0;
  }
}

class Box {
  int touch() { return Mark.boxed(); }
}

class Handles {
  private int priv() { return Mark.bound(); }

  int virt() { return Mark.virt(); }

  static int statik(String s) { return Mark.stat(); }

  static int kinds() {
    Handles handles = new Handles();
    List<String> list = new ArrayList<>();
    Function<String, Integer> fixed = Handles::statik;
    Supplier<Integer> virtual = handles::virt;
    Supplier<Integer> special = handles::priv;
    Function<List<String>, Integer> interfaced = List::size;
    Supplier<Box> created = Box::new;
    return fixed.apply("") + virtual.get() + special.get() + interfaced.apply(list) + created.get().touch();
  }
}

interface Marker {
  default int mark() { return Mark.marked(); }
}

interface Wide {
  Object get();
}

interface Slim {
  String get();
}

interface Joint extends Wide, Slim {}

class Alternate {
  static int markers() {
    Runnable run = (Runnable & Marker) () -> Mark.ran();
    run.run();
    return ((Marker) run).mark();
  }

  static int bridges() {
    Joint joint = () -> { Mark.bridged(); return ""; };
    Wide wide = joint;
    return wide.get() == null ? 0 : 1;
  }
}

class SBase {
  static int X = Mark.base();
}

class SSub extends SBase {
  static { Mark.sub(); }
}

interface WithDefault {
  int V = Mark.defaulted();

  default int d() { return 1; }
}

interface PlainI {
  int P = Mark.plain();
}

class Both implements WithDefault, PlainI {}

class Statics {
  static { Mark.work(); }

  static int go() { return 1; }
}

class Inits {
  static int readViaSub() { return SSub.X; }

  static Object construct() { return new SSub(); }

  static Object implement() { return new Both(); }

  static int staticCall() { return Statics.go(); }
}

class Arrays {
  static int[] ints(int[] a) { return a.clone(); }

  static Object[] objects(Object[] a) { return a.clone(); }
}

class Worker extends Thread {
  public void run() { Mark.work(); }
}

class Idle extends Thread {
  public void run() { Mark.threaded(); }
}

class Threads {
  static Object spawn() { return new Worker(); }

  static Object wrapped() { return new Thread(() -> Mark.ran()); }
}

class Named {
  public String toString() { return "n" + Mark.named(); }
}

class Concat {
  static String join() { return "n=" + new Named(); }
}

class Gone {
  static int m() { return 0; }
}

class Edge {
  native int nat();

  static int entry() {
    Gone.m();
    return new Edge().nat() + Mark.circle();
  }
}

class Calls {
  static int deep() { return Mark.circle(); }

  static int entry() { return deep(); }
}
"""

  private val mid = """package fixture

class SMid extends JBase {
  override def go(): Int = Mark.sub()
}

trait STrait {
  def hook(): Int = Mark.hook()
}

class Companion {
  def m(x: Int): Int = Mark.circle() + x
}

object Companion {
  def m(x: Int): Int = x + 1
  def forwarded(): Int = Mark.square()
}

object Collide {
  def run(): Int = Companion.m(1)
}

object Config {
  val x: Int = Mark.hello()
}

object UseConfig {
  def read(): Int = Config.x
}

object Funs {
  private def generic[A, B](g: A => B, a: A): B = g(a)

  def run(): Int = generic[Int, Int](x => Mark.stat() + x, 1)
}

class Capturing {
  private val captured = 1
  def run(): Int = {
    val f: () => Int = () => captured + Mark.ran()
    f()
  }
}

object Entry {
  def main(args: Array[String]): Unit = println(Collide.run())
}

object Launcher extends App {
  println(Mark.hello())
}

class Outer {
  class Inner(v: Int) {
    def go(): Int = v
  }
}

object Holder {
  class Nested {
    def go(): Int = Mark.dead()
  }
}
"""

  private val leaf = """package fixture;

public class Leaf extends SMid implements STrait {}
"""

  private val cross = """package fixture;

public class Cross {
  public static int run() {
    JBase b = new Leaf();
    return b.go() + b.base() + ((STrait) b).hook();
  }

  public static int forwarded() {
    return Companion.forwarded();
  }
}
"""

  private lazy val built: Vector[Path] = {
    val directory = Scratch.directory("bonsai-call-graph-")
    val first     = Files.createDirectories(directory.resolve("java-first"))
    write(
      directory.resolve("src-first"),
      Seq(
        "fixture/Mark.java"     -> mark,
        "fixture/JBase.java"    -> base,
        "fixture/Provider.java" -> provider,
        "fixture/Api.java"      -> api,
        "fixture/Shapes.java"   -> shapes
      )
    )
    javac(
      Seq("Mark", "JBase", "Provider", "Api", "Shapes").map(name =>
        directory.resolve("src-first").resolve(s"fixture/$name.java")
      ),
      first,
      classpath
    ): Unit
    val scala  = compile(directory.resolve("scala"), Seq("Mid.scala" -> mid), Vector(first))
    val second = Files.createDirectories(directory.resolve("java-second"))
    write(directory.resolve("src-second"), Seq("fixture/Leaf.java" -> leaf, "fixture/Cross.java" -> cross))
    javac(
      Seq("Leaf", "Cross").map(name => directory.resolve("src-second").resolve(s"fixture/$name.java")),
      second,
      classpath ++ Vector(first, scala)
    ): Unit
    Vector(first, scala, second)
  }

  private val platform = ClassLoader.getPlatformClassLoader

  private def jdk(name: String): Option[Array[Byte]] =
    Option(platform.getResourceAsStream(s"$name.class")).map(stream => Using.resource(stream)(_.readAllBytes()))

  private def withWorld[A](absent: Set[String] = Set.empty)(body: ClassWorld => A): A =
    ClassSource.of(built, classpath, None) match {
      case Left(error)   => fail(error)
      case Right(source) =>
        Using.resource(source) { source =>
          body(ClassWorld(name => if (absent(name)) None else source.bytes(name).orElse(jdk(name))))
        }
    }

  private def ref(owner: String, name: String, descriptor: String = "()I"): Reference =
    Reference(s"fixture/$owner", name, descriptor)

  private def graphOf[A](roots: Reference*)(check: CallGraph => A): A =
    withWorld()(world => check(CallGraph.build(world, roots.toSet)))

  private def ours(graph: CallGraph): Map[CallGraph.LambdaSite, Lambda] =
    graph.lambdas.filter((site, _) => site.holder.owner.startsWith("fixture/"))

  private def assertReaches(graph: CallGraph, expected: Reference*): Unit =
    assertEquals(expected.filterNot(graph.reachable).toVector, Vector.empty[Reference])

  private def assertMisses(graph: CallGraph, unexpected: Reference*): Unit =
    assertEquals(unexpected.filter(graph.reachable).toVector, Vector.empty[Reference])

  test("an override joins once its class is instantiated, whichever of the site and the instantiation comes first") {
    for (entry <- Seq("siteThenNew", "newThenSite"))
      graphOf(ref("Orders", entry, "(Lfixture/Shape;)I")) { found =>
        assertReaches(found, ref("Square", "area"), ref("Mark", "square"))
        assertMisses(found, ref("Circle", "area"), ref("Mark", "circle"))
        assert(found.instantiated.contains("fixture/Square"), entry)
      }
  }

  test("an override of a class that is never instantiated stays out") {
    graphOf(ref("Orders", "circles")) { found =>
      assertReaches(found, ref("Circle", "area"), ref("Mark", "circle"))
      assertMisses(found, ref("Square", "area"), ref("Mark", "square"), ref("Shape", "area"))
      assert(!found.instantiated.contains("fixture/Square"))
    }
  }

  test("a method called only from a dead method of a live type is unreachable") {
    graphOf(ref("Dead", "run")) { found =>
      assertReaches(found, ref("Live", "used"), ref("Live", "<init>", "()V"))
      assertMisses(found, ref("Live", "unused"), ref("Mark", "dead"))
    }
  }

  test("a call edge runs from the holder to the method it reaches") {
    graphOf(ref("Calls", "entry")) { found =>
      assertEquals(found.edges(ref("Calls", "entry")), Set(ref("Calls", "deep")))
      assertEquals(found.edges(ref("Calls", "deep")), Set(ref("Mark", "circle")))
    }
  }

  test("a Java class extends a Scala class that extends a Java class, and implements a Scala trait") {
    graphOf(Reference("fixture/Cross", "run", "()I")) { found =>
      assertReaches(
        found,
        Reference("fixture/SMid", "go", "()I"),
        Reference("fixture/JBase", "base", "()I"),
        Reference("fixture/STrait", "hook", "()I"),
        ref("Mark", "sub"),
        ref("Mark", "base"),
        ref("Mark", "hook")
      )
      assertMisses(found, Reference("fixture/JBase", "go", "()I"))
    }
  }

  test("a static forwarder reaches the object, and the object method stays apart from the class method of its key") {
    graphOf(Reference("fixture/Cross", "forwarded", "()I"), Reference("fixture/Collide$", "run", "()I")) { found =>
      assertReaches(
        found,
        Reference("fixture/Companion", "forwarded", "()I"),
        Reference("fixture/Companion$", "forwarded", "()I"),
        Reference("fixture/Companion$", "m", "(I)I"),
        ref("Mark", "square")
      )
      assertMisses(found, Reference("fixture/Companion", "m", "(I)I"), ref("Mark", "circle"))
    }
    withWorld() { world =>
      assertEquals(
        world.key(Reference("fixture/Companion", "m", "(I)I")),
        world.key(Reference("fixture/Companion$", "m", "(I)I"))
      )
    }
  }

  test("the key of a constructor of an inner class leaves the enclosing instance out") {
    withWorld() { world =>
      assertEquals(
        world.key(Reference("fixture/Outer$Inner", "<init>", "(Lfixture/Outer;I)V")),
        "fixture.Outer.Inner.<init>(I)V"
      )
    }
  }

  test("a default method is reached through an instance that inherits it, not through one that overrides it") {
    graphOf(ref("Defaults", "entry")) { found =>
      assertReaches(found, ref("Greeter", "greet"), ref("Mark", "hello"))
      assertMisses(found, ref("Over", "greet"), ref("Mark", "over"))
    }
  }

  test("a lambda runs through the interface method of a call, and only a reachable holder creates it") {
    graphOf(ref("Lambdas", "invoke")) { found =>
      val implementation = ours(found).values.map(l => Reference(l.owner, l.name, l.descriptor)).toSet
      assertEquals(implementation.size, 1)
      assertReaches(found, implementation.head, ref("Mark", "ran"))
      assertEquals(ours(found).keySet.map(_.holder), Set(ref("Lambdas", "invoke")))
    }
    graphOf(ref("Lambdas", "callOnly", "(Ljava/lang/Runnable;)I")) { found =>
      assertMisses(found, ref("Mark", "ranDead"), ref("Mark", "ran"))
      assertEquals(ours(found), Map.empty)
    }
  }

  test("each lambda site is an instance of its own that the same interface call reaches") {
    graphOf(ref("Lambdas", "twice")) { found =>
      assertEquals(ours(found).keySet.map(_.position), Set(0, 1))
      assertReaches(found, ref("Mark", "ran"), ref("Mark", "stat"))
    }
  }

  test("method handles of every kind are implementations, and a constructor reference instantiates its class") {
    graphOf(ref("Handles", "kinds")) { found =>
      val tags = found.lambdas.collect { case (site, lambda) if site.holder.owner == "fixture/Handles" => lambda.tag }
      assert(
        Set(Opcodes.H_INVOKESTATIC, Opcodes.H_INVOKEVIRTUAL, Opcodes.H_INVOKEINTERFACE, Opcodes.H_NEWINVOKESPECIAL)
          .subsetOf(tags.toSet),
        tags.toString
      )
      assert(found.instantiated.contains("fixture/Box"))
      assertReaches(
        found,
        ref("Handles", "statik", "(Ljava/lang/String;)I"),
        ref("Handles", "virt"),
        ref("Handles", "priv"),
        ref("Box", "<init>", "()V"),
        ref("Box", "touch"),
        Reference("java/util/ArrayList", "size", "()I")
      )
      assertReaches(found, ref("Mark", "stat"), ref("Mark", "virt"), ref("Mark", "bound"), ref("Mark", "boxed"))
    }
  }

  test("a special method handle is an implementation") {
    graphOf(Reference("fixture/Capturing", "run", "()I")) { found =>
      assertEquals(ours(found).values.map(_.tag).toSet, Set(Opcodes.H_INVOKESPECIAL))
      assertReaches(found, ref("Mark", "ran"))
    }
  }

  test("marker interfaces of a lambda give it their default methods") {
    graphOf(ref("Alternate", "markers")) { found =>
      assertEquals(ours(found).values.map(_.markers).toSet, Set(Vector("fixture/Marker")))
      assertReaches(found, ref("Marker", "mark"), ref("Mark", "marked"), ref("Mark", "ran"))
    }
  }

  test("a bridge of a lambda reaches the implementation from the interface it overrides") {
    graphOf(ref("Alternate", "bridges")) { found =>
      assertEquals(ours(found).values.map(_.bridges).toSet, Set(Vector("()Ljava/lang/Object;")))
      assertReaches(found, ref("Mark", "bridged"))
    }
  }

  test("a call to Function1.apply selects the default of a specialised lambda interface, which reaches the lambda") {
    graphOf(Reference("fixture/Funs$", "run", "()I")) { found =>
      val lambda =
        found.lambdas.collectFirst { case (site, closure) if site.holder.owner == "fixture/Funs$" => closure }.get
      val default  = Reference(lambda.interface, "apply", "(Ljava/lang/Object;)Ljava/lang/Object;")
      val sam      = Reference(lambda.interface, lambda.sam, lambda.samDescriptor)
      val function = Reference("scala/Function1", "apply", "(Ljava/lang/Object;)Ljava/lang/Object;")
      assertEquals(lambda.interface, "scala/runtime/java8/JFunction1$mcII$sp")
      assert(found.reachable(default))
      assert(!found.reachable(sam))
      assert(!found.reachable(function))
      assert(found.edges(default).contains(Reference(lambda.owner, lambda.name, lambda.descriptor)))
      assertReaches(found, ref("Mark", "stat"))
    }
  }

  test("class initialisers run for a new, a static call and the declaring class of a static field") {
    graphOf(ref("Inits", "readViaSub")) { found =>
      assertReaches(found, Reference("fixture/SBase", "<clinit>", "()V"), ref("Mark", "base"))
      assertMisses(found, Reference("fixture/SSub", "<clinit>", "()V"), ref("Mark", "sub"))
    }
    graphOf(ref("Inits", "construct", "()Ljava/lang/Object;")) { found =>
      assertReaches(found, Reference("fixture/SBase", "<clinit>", "()V"), Reference("fixture/SSub", "<clinit>", "()V"))
      assertReaches(found, ref("Mark", "base"), ref("Mark", "sub"))
    }
    graphOf(ref("Inits", "staticCall")) { found =>
      assertReaches(found, Reference("fixture/Statics", "<clinit>", "()V"), ref("Mark", "work"))
    }
  }

  test("a class initialises the superinterfaces that declare defaults, and not the others") {
    graphOf(ref("Inits", "implement", "()Ljava/lang/Object;")) { found =>
      assertReaches(found, Reference("fixture/WithDefault", "<clinit>", "()V"), ref("Mark", "defaulted"))
      assertMisses(found, Reference("fixture/PlainI", "<clinit>", "()V"), ref("Mark", "plain"))
    }
  }

  test("a Scala object initialises when it is first read") {
    graphOf(Reference("fixture/UseConfig$", "read", "()I")) { found =>
      assertReaches(found, Reference("fixture/Config$", "<clinit>", "()V"), ref("Mark", "hello"))
    }
  }

  test("an array clone resolves through Object") {
    graphOf(ref("Arrays", "ints", "([I)[I"), ref("Arrays", "objects", "([Ljava/lang/Object;)[Ljava/lang/Object;")) {
      found =>
        assertReaches(found, Reference("java/lang/Object", "clone", "()Ljava/lang/Object;"))
        assertEquals(found.unresolved.filter(_.owner.startsWith("[")), Set.empty[Reference])
    }
  }

  test("a started thread runs its run method, and the runnable it was given is reached through Thread.run") {
    graphOf(ref("Threads", "spawn", "()Ljava/lang/Object;")) { found =>
      assertReaches(found, ref("Worker", "run", "()V"), ref("Mark", "work"))
      assertMisses(found, ref("Idle", "run", "()V"), ref("Mark", "threaded"))
    }
    graphOf(ref("Threads", "wrapped", "()Ljava/lang/Object;")) { found =>
      assertEquals(ours(found).size, 1)
      assertReaches(found, ref("Mark", "ran"))
    }
  }

  test("string concatenation reaches the toString of the objects it joins") {
    graphOf(ref("Concat", "join", "()Ljava/lang/String;")) { found =>
      assertReaches(found, ref("Named", "toString", "()Ljava/lang/String;"), ref("Mark", "named"))
    }
  }

  test("native methods and missing classes are leaves, and an unresolved reference does not stop the walk") {
    withWorld(Set("fixture/Gone")) { world =>
      val found = CallGraph.build(world, Set(ref("Edge", "entry")))
      assertEquals(found.unresolved.filter(_.owner == "fixture/Gone"), Set(ref("Gone", "m")))
      assertReaches(found, ref("Edge", "nat"), ref("Mark", "circle"))
      assertEquals(found.edges.get(ref("Edge", "nat")), None)
    }
  }

  test("a class the scan refuses is a leaf") {
    withWorld() { world =>
      val found = CallGraph.build(world, Set(ref("Calls", "entry")), _ != "fixture/Calls")
      assertReaches(found, ref("Calls", "entry"))
      assertMisses(found, ref("Calls", "deep"), ref("Mark", "circle"))
    }
  }

  private def rootsOf(names: Seq[String], exported: Set[String] = Set.empty): Set[Reference] =
    withWorld() { world =>
      val nodes = names.map { name =>
        name -> Node(name, "m", Output.Scope.Main, "f", 1, generated = false, exported = exported(name), outer = None)
      }
      CallGraph.roots(Graph(nodes.toMap, Map.empty, names.toSet), world)
    }

  test("a type with a main method roots it, through the forwarder of an object or the trait of an App") {
    val main = "([Ljava/lang/String;)V"
    assertEquals(rootsOf(Seq("fixture.Entry")), Set(Reference("fixture/Entry", "main", main)))
    assertEquals(rootsOf(Seq("fixture.Launcher")).map(_.name), Set("main"))
  }

  test("a type the build names to a loader roots its public constructors, overrides and provider method") {
    assertEquals(
      rootsOf(Seq("fixture.Provider")),
      Set(
        Reference("fixture/Provider", "<init>", "()V"),
        Reference("fixture/Provider", "run", "()V"),
        Reference("fixture/Provider", "provider", "()Lfixture/Provider;")
      )
    )
  }

  test("an exported type roots its public and protected members") {
    assertEquals(
      rootsOf(Seq("fixture.Api"), Set("fixture.Api")),
      Set(
        Reference("fixture/Api", "<init>", "()V"),
        Reference("fixture/Api", "open", "()I"),
        Reference("fixture/Api", "guarded", "()I")
      )
    )
  }

  test("a nested type is found by the dotted name of its node, and an object by its module class") {
    assertEquals(
      rootsOf(Seq("fixture.Outer.Inner"), Set("fixture.Outer.Inner")).map(_.owner),
      Set("fixture/Outer$Inner")
    )
    assertEquals(
      rootsOf(Seq("fixture.Holder.Nested"), Set("fixture.Holder.Nested")).map(_.owner),
      Set("fixture/Holder$Nested")
    )
    assert(rootsOf(Seq("fixture.Config")).contains(Reference("fixture/Config$", "<clinit>", "()V")))
  }

  test("a root that is an instance method makes its class instantiated") {
    graphOf(Reference("fixture/Provider", "run", "()V")) { found =>
      assert(found.instantiated.contains("fixture/Provider"))
    }
  }
}
