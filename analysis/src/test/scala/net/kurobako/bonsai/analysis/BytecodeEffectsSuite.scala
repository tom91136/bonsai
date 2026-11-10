package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.analysis.ClassFiles.*
import net.kurobako.bonsai.effects.Kind as EffectKind
import net.kurobako.bonsai.java.Model as JavaModel
import net.kurobako.bonsai.testing.*
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes
import scala.jdk.CollectionConverters.*
import scala.util.Using

class BytecodeEffectsSuite extends AnalysisSuite {
  private val sources = Seq(
    "fixture/Box.java" -> """package fixture;

public class Box {
  public int value;
  public Box() {}
  public Box(int v) { this.value = v; }
  public void set(int v) { this.value = v; }
  public int get() { return value; }
}
""",
    "fixture/Alloc.java" -> """package fixture;

public class Alloc {
  static int counter;

  public static int local() { Box b = new Box(); b.value = 3; return b.value; }
  public static Box make() { Box b = new Box(); b.set(7); return b; }
  public static int built() { return new Box(5).value; }
  public static void param(Box b) { b.value = 1; }
  public static void viaCall(Box b) { b.set(1); }
  public static int fresh() { Box b = new Box(); b.set(2); return b.get(); }
  public static void bump() { counter++; }
  public static void store(int[] a) { a[0] = 1; }
  public static int load(int[] a) { return a[0]; }
  public static int temporary() { int[] a = new int[4]; a[0] = 1; return a[0]; }
  public static native int nat();
  public static int callNative() { return nat(); }
  public synchronized void locking() {}
  public void blockThis() { synchronized (this) { } }
  public static void blockOther(Object o) { synchronized (o) { } }
  public static void blockFresh() { Object o = new Object(); synchronized (o) { } }
  public static Runnable lambda() { return () -> counter++; }
  public static Runnable quiet() { return () -> { }; }
  public static void runIt(Runnable r) { r.run(); }
}
""",
    "fixture/Shape.java" -> """package fixture;

public abstract class Shape {
  public abstract int area();
}
""",
    "fixture/Square.java" -> """package fixture;

public class Square extends Shape {
  public int area() { return 4; }
}
""",
    "fixture/Dirty.java" -> """package fixture;

public class Dirty extends Shape {
  public int area() { Alloc.counter++; return 9; }
}
""",
    "fixture/Shapes.java" -> """package fixture;

public class Shapes {
  public static int total(Shape s) { return s.area(); }
  public static Shape square() { return new Square(); }
  public static Shape dirty() { return new Dirty(); }
}
""",
    "fixture/Cycle.java" -> """package fixture;

public class Cycle {
  static int hits;

  public static boolean even(int n) { return n == 0 ? true : odd(n - 1); }
  public static boolean odd(int n) { if (n == 0) return false; hits++; return even(n - 1); }
  public static int fact(int n) { return n <= 1 ? 1 : n * fact(n - 1); }
  public static int ping(int n) { return n == 0 ? 0 : pong(n - 1); }
  public static int pong(int n) { return n == 0 ? 0 : pang(n - 1); }
  public static int pang(int n) { return n == 0 ? 0 : ping(n - 1); }
  public static int entry(int n) { return ping(n) + leaf(n); }
  public static int leaf(int n) { return n + 1; }
  public static int after(int n) { return leaf(n) * 2; }
}
""",
    "org/slf4j/Quiet.java" -> """package org.slf4j;

public class Quiet {
  public static int info(String s) { return s.length(); }
}
""",
    "scala/runtime/Loud.java" -> """package scala.runtime;

public class Loud {
  static int n;
  public static int bump() { return n++; }
}
""",
    "fixture/Authority.java" -> """package fixture;

public class Authority {
  public static int logs() { return org.slf4j.Quiet.info("x"); }
  public static int loud() { return scala.runtime.Loud.bump(); }
}
""",
    "fixture/Jdk.java" -> """package fixture;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;

public class Jdk {
  public static boolean add(ArrayList<String> l) { return l.add("x"); }
  public static Object get(ArrayList<String> l) { return l.get(0); }
  public static java.util.List<String> listed(String[] a) { return Arrays.asList(a); }
  public static Object random() { return ThreadLocalRandom.current(); }
}
""",
    "fixture/Cached.java" -> """package fixture;

public class Cached {
  private int hash;

  public int hashCode() {
    int h = hash;
    if (h == 0) { h = 31; hash = h; }
    return h;
  }

  public static int viaParam(Cached c) { return c.hashCode(); }
  public static int viaFresh() { return new Cached().hashCode(); }
}
""",
    "fixture/Registry.java" -> """package fixture;

public class Registry {
  static final String HOME = System.getProperty("user.home");
  static int created;

  static { created = 1; }

  public static String home() { return HOME; }
}
""",
    "fixture/Table.java" -> """package fixture;

public class Table {
  static final int[] SQUARES = new int[8];
  static int filled;

  static {
    for (int i = 0; i < SQUARES.length; i++) SQUARES[i] = i * i;
    filled = 1;
  }

  public static int at(int i) { return SQUARES[i]; }
  public static void poke(int i) { SQUARES[i] = 1; }
}
""",
    "fixture/Poker.java" -> """package fixture;

public class Poker {
  static { Table.SQUARES[0] = 5; }
}
""",
    "fixture/Handles.java" -> """package fixture;

import java.util.function.Supplier;

public class Handles {
  public static Supplier<Integer> getter(Box b) { return b::get; }
  public static int call(Supplier<Integer> s) { return s.get(); }
}
""",
    "fixture/Users.java" -> """package fixture;

public class Users {
  public static String home() { return Registry.home(); }
  public static int table() { return Table.at(2); }
  public static Object made() { return new Table(); }
}
""",
    "fixture/Bounds.java" -> """package fixture;

public class Bounds {
  public static int constant(int a) { return a + 1; }
  public static int looped(int[] a) { int s = 0; for (int x : a) s += x; return s; }
  public static int callsLoop(int[] a) { return looped(a); }
  public static int callsConstant(int a) { return constant(a) * 2; }
  public static int allocates(int n) { return new int[n].length; }
  public static int recursive(int n) { return n == 0 ? 0 : recursive(n - 1); }
  public static int failing(int[] a, int i) {
    if (i < 0) {
      for (int k = 0; k < 10; k++) a[k]++;
      throw new IllegalStateException(String.valueOf(i));
    }
    return i;
  }
  public static String joined(String a, String b) { return a + b; }
}
""",
    "fixture/Lists.java" -> """package fixture;

import java.util.ArrayList;
import java.util.List;

public class Lists {
  public static List<String> make() { return new ArrayList<>(); }
  public static boolean contains(List<String> l, Object o) { return l.contains(o); }
  public static Object get(List<String> l) { return l.get(0); }
}
""",
    "fixture/Gone.java" -> """package fixture;

public class Gone {
  public static int m() { return 1; }
}
""",
    "fixture/Missing.java" -> """package fixture;

public class Missing {
  public static int call() { return Gone.m(); }
  public static int guarded(int i) {
    if (i < 0) throw new IllegalStateException(String.valueOf(Gone.m()));
    return i;
  }
}
""",
    "fixture/Point.java" -> """package fixture;

public record Point(int x, int y) {}
""",
    "fixture/Dyn.java" -> """package fixture;

public class Dyn {
  public static String show(Point p) { return p.toString(); }
}
""",
    "fixture/Refl.java" -> """package fixture;

public class Refl {
  public static Object call(java.lang.reflect.Method m, Object o) throws Exception { return m.invoke(o); }
}
""",
    "scala/Predef$.java" -> """package scala;

public class Predef$ {
  public void println(Object o) { }
}
""",
    "scala/runtime/Boot.java" -> """package scala.runtime;

public class Boot {
  static String home = System.getProperty("user.home");
  static int made;

  public Boot() { made++; }
}
""",
    "fixture/Updating.java" -> """package fixture;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

public class Updating {
  static final AtomicReferenceFieldUpdater<Updating, Object> LINK =
      AtomicReferenceFieldUpdater.newUpdater(Updating.class, Object.class, "link");
  static final AtomicIntegerFieldUpdater<Updating> SIZE = AtomicIntegerFieldUpdater.newUpdater(Updating.class, "size");
  static final VarHandle COUNT;

  static {
    try {
      COUNT = MethodHandles.lookup().findVarHandle(Updating.class, "count", int.class);
    } catch (ReflectiveOperationException e) {
      throw new ExceptionInInitializerError(e);
    }
  }

  volatile Object link;
  volatile int size;
  volatile int count;

  public boolean casLink(Object a, Object b) { return LINK.compareAndSet(this, a, b); }
  public boolean casSize(int a, int b) { return SIZE.compareAndSet(this, a, b); }
  public boolean casCount(int a, int b) { return COUNT.compareAndSet(this, a, b); }
  public int readSize() { return SIZE.get(this); }
}
""",
    "fixture/Atoms.java" -> """package fixture;

import java.util.concurrent.atomic.AtomicInteger;

public class Atoms {
  public static boolean cas(AtomicInteger cell) { return cell.compareAndSet(0, 1); }
  public static int bump(Holding holding) { return holding.cell.incrementAndGet(); }
  public static int read(AtomicInteger cell) { return cell.get(); }

  public static class Holding {
    public final AtomicInteger cell = new AtomicInteger();
  }
}
""",
    "fixture/Cell.java" -> """package fixture;

public class Cell {
  static int total;
  int n;

  public Cell() {}
  public Cell(int v) { setN(v); }
  void setN(int v) { n = v; }
  public void tick() { total++; }
  public void both() { n = 1; total++; }

  public static void freshTick() { new Cell().tick(); }
  public static void paramTick(Cell c) { c.tick(); }
  public static void freshBoth() { new Cell().both(); }
  public static void paramBoth(Cell c) { c.both(); }
  public static Cell build(int v) { return new Cell(v); }
  public static void maybe(boolean c, Cell p) { Cell b = c ? new Cell() : p; b.n = 1; }
  public static void maybeSwapped(boolean c, Cell p) { Cell b = c ? p : new Cell(); b.n = 1; }
  public static void bothFresh(boolean c) { Cell b = c ? new Cell() : new Cell(); b.n = 1; }
  public static synchronized void locked() {}
}
""",
    "fixture/Counted.java" -> """package fixture;

public class Counted {
  static int made;

  public Counted() { made++; }

  public static Counted make() { return new Counted(); }
}
""",
    "fixture/Job.java" -> """package fixture;

public abstract class Job {
  public abstract int run();
}
""",
    "fixture/Failing.java" -> """package fixture;

public class Failing extends Job {
  public int run() { throw new IllegalStateException(); }
}
""",
    "fixture/Silent.java" -> """package fixture;

public class Silent extends Job {
  public int run() { return 1; }
}
""",
    "fixture/Jobs.java" -> """package fixture;

public class Jobs {
  public static int run(Job j) { return j.run(); }
  public static Job failing() { return new Failing(); }
  public static Job silent() { return new Silent(); }
}
""",
    "fixture/Twin.java" -> """package fixture;

public class Twin {
  public static int m(int[] a) { return a.length; }
}
""",
    "fixture/Twin$.java" -> """package fixture;

public class Twin$ {
  public static int m(int[] a) { int s = 0; for (int x : a) s += x; return s; }
}
""",
    "fixture/Ranked.java" -> """package fixture;

public class Ranked implements Comparable<Ranked> {
  int rank;

  public int compareTo(Ranked o) { return rank - o.rank; }
  public boolean equals(Object o) { return o == this; }
  public int hashCode() { return rank; }
  public int hashCode(int seed) { return seed; }
}
""",
    "fixture/Ordering.java" -> """package fixture;

public class Ordering implements java.util.Comparator<Ranked> {
  public int compare(Ranked a, Ranked b) { return a.rank - b.rank; }
}
""",
    "fixture/Plain.java" -> """package fixture;

public class Plain {
  public int compare(int a, int b) { return a - b; }
  public int compareTo(Plain o) { return 0; }
}
""",
    "fixture/Holder.java" -> """package fixture;

public class Holder {
  public static class Nested {
    public static int m() { return 1; }
  }
}
""",
    "fixture/GoneField.java" -> """package fixture;

public class GoneField {
  public static int x;
}
""",
    "fixture/Peeks.java" -> """package fixture;

public class Peeks {
  public static int created() { return Registry.created; }
  public static int absent() { return GoneField.x; }
  public static Object konst() { return new Konsts(); }
}
""",
    "fixture/Konst.java" -> """package fixture;

public interface Konst {
  Object ZERO = new Object();
}
""",
    "fixture/Konsts.java" -> """package fixture;

public class Konsts implements Konst {
  public static Konsts make() { return new Konsts(); }
}
""",
    "fixture/Name.java" -> """package fixture;

public class Name {
  String text = "x";

  public String text() { return text; }
}
""",
    "fixture/Source.java" -> """package fixture;

public interface Source<T> {
  T get();
}
""",
    "fixture/Text.java" -> """package fixture;

public interface Text extends Source<String> {
  String get();
}
""",
    "fixture/Tag.java" -> """package fixture;

public interface Tag {}
""",
    "fixture/Sized.java" -> """package fixture;

public interface Sized {
  int size();
}
""",
    "fixture/Fixed.java" -> """package fixture;

public class Fixed implements Sized {
  int n;

  public int size() { return n; }
}
""",
    "fixture/Sizing.java" -> """package fixture;

import java.util.function.Supplier;

public class Sizing {
  public static Supplier<Integer> sizer(Sized s) { return s::size; }
  public static Sized fixed() { return new Fixed(); }
  public static int pull(Supplier<Integer> s) { return s.get(); }
}
""",
    "fixture/Pulls.java" -> """package fixture;

import java.util.function.Supplier;

public class Pulls {
  public static Text bound(Name n) { return n::text; }
  public static Supplier<String> tagged(Name n) { return (Supplier<String> & Tag) n::text; }
  public static Object pull(Source<String> s) { return s.get(); }
  public static String pullText(Text t) { return t.get(); }
  public static String pullSupplier(Supplier<String> s) { return s.get(); }
  public static Name name() { return new Name(); }
}
"""
  )

  private lazy val built: Vector[Path] = {
    val directory = Scratch.directory("bonsai-bytecode-effects-")
    val classes   = Files.createDirectories(directory.resolve("classes"))
    write(directory.resolve("src"), sources)
    javac(sources.map((name, _) => directory.resolve("src").resolve(name)), classes, classpath): Unit
    Vector(classes)
  }

  private val platform = ClassLoader.getPlatformClassLoader

  private def jdk(name: String): Option[Array[Byte]] =
    Option(platform.getResourceAsStream(s"$name.class")).map(stream => Using.resource(stream)(_.readAllBytes()))

  private def classesOf(directory: Path): Vector[String] =
    Using.resource(Files.walk(directory))(_.toList.asScala.toVector).filter(_.toString.endsWith(".class")).map(file =>
      directory.relativize(file).toString.stripSuffix(".class")
    )

  private def ref(owner: String, name: String, descriptor: String = "()I"): Reference =
    Reference(s"fixture/$owner", name, descriptor)

  private def methodsOf(world: ClassWorld, names: Seq[String]): Seq[Reference] =
    names.flatMap(name =>
      world.get(name).toSeq.flatMap(_.methods.filterNot(_.isAbstract).map(method =>
        Reference(name, method.name, method.descriptor)
      ))
    )

  /** The summaries over a world of the fixtures, rooted at every method of `outputs` and at `extra`. */
  private def summarised[A](
      extra: Seq[Reference] = Nil,
      absent: Set[String] = Set.empty,
      outputs: Vector[Path] = built,
      rooted: String => Boolean = _ => true
  )(check: (ClassWorld, BytecodeEffects.Summaries) => A): A =
    ClassSource.of(outputs, classpath, None) match {
      case Left(error)   => fail(error)
      case Right(source) =>
        Using.resource(source) { source =>
          val world = ClassWorld(name => if (absent(name)) None else source.bytes(name).orElse(jdk(name)))
          val names = outputs.flatMap(classesOf(_)).filter(rooted)
          val graph = CallGraph.build(world, (methodsOf(world, names) ++ extra).toSet)
          val found = BytecodeEffects.summarise(world, graph)
          check(world, found)
        }
    }

  private def of(world: ClassWorld, summaries: BytecodeEffects.Summaries, reference: Reference) =
    summaries.get(world.key(reference)).getOrElse(fail(s"no summary for ${world.key(reference)}"))

  private def kinds(summary: BytecodeEffects.Behaviour): Set[EffectKind] = summary.effects.map(_.kind)

  private def isPure(summary: BytecodeEffects.Behaviour): Boolean =
    Purity.verdictOf(Purity.ranked(summary.effects)) == Purity.Verdict.Pure

  private def traced(file: Path, owner: String, name: String): Trace = {
    val bytes  = Files.readAllBytes(file.resolve(s"$owner.class"))
    val method = ClassFiles.read(bytes).toOption.get.methods.find(_.name == name).get
    ClassFiles.trace(bytes, owner, Vector(method))((method.name, method.descriptor))
  }

  test("a write to an object the method allocated is invisible, a write through a parameter is not") {
    summarised() { (world, summaries) =>
      def at(name: String, descriptor: String = "()I") = of(world, summaries, ref("Alloc", name, descriptor))
      assert(isPure(at("local")))
      assert(isPure(at("make", "()Lfixture/Box;")))
      assert(isPure(at("built")))
      assert(isPure(at("temporary")))
      assert(isPure(at("fresh")))
      assertEquals(kinds(at("param", "(Lfixture/Box;)V")), Set(EffectKind.Writes))
      assertEquals(kinds(at("viaCall", "(Lfixture/Box;)V")), Set(EffectKind.Writes))
      assert(at("viaCall", "(Lfixture/Box;)V").effects.forall(effect => !effect.onThis && !effect.assumed))
      assertEquals(kinds(at("store", "([I)V")), Set(EffectKind.Writes))
    }
  }

  test("a method that writes this acts on its receiver, which a fresh receiver hides and a constructor owns") {
    summarised() { (world, summaries) =>
      val set = of(world, summaries, ref("Box", "set", "(I)V"))
      assertEquals(set.effects.map(effect => (effect.kind, effect.onThis)), Set((EffectKind.Writes, true)))
      assert(of(world, summaries, ref("Box", "<init>", "(I)V")).effects.isEmpty)
      assert(of(world, summaries, ref("Box", "get")).effects.forall(_.onThis))
    }
  }

  test("a write to a static field is an effect") {
    summarised() { (world, summaries) =>
      val bump = of(world, summaries, ref("Alloc", "bump", "()V"))
      assertEquals(kinds(bump), Set(EffectKind.Writes, EffectKind.Reads))
      assert(bump.effects.forall(!_.onThis))
    }
  }

  test("a write through a field updater, a variable handle or an atomic is an effect") {
    summarised() { (world, summaries) =>
      def at(owner: String, name: String, descriptor: String) = of(world, summaries, ref(owner, name, descriptor))
      assert(kinds(at("Updating", "casLink", "(Ljava/lang/Object;Ljava/lang/Object;)Z")).contains(EffectKind.Writes))
      assert(kinds(at("Updating", "casSize", "(II)Z")).contains(EffectKind.Writes))
      assert(!isPure(at("Updating", "casCount", "(II)Z")))
      assertEquals(kinds(at("Atoms", "cas", "(Ljava/util/concurrent/atomic/AtomicInteger;)Z")), Set(EffectKind.Writes))
      assertEquals(kinds(at("Atoms", "bump", "(Lfixture/Atoms$Holding;)I")), Set(EffectKind.Writes))
      assertEquals(kinds(at("Atoms", "read", "(Ljava/util/concurrent/atomic/AtomicInteger;)I")), Set(EffectKind.Reads))
    }
  }

  test("a native method is unknown, and so is a caller of it") {
    summarised() { (world, summaries) =>
      assertEquals(kinds(of(world, summaries, ref("Alloc", "nat"))), Set(EffectKind.Unknown))
      assert(kinds(of(world, summaries, ref("Alloc", "callNative"))).contains(EffectKind.Unknown))
      assert(!of(world, summaries, ref("Alloc", "nat")).bounded)
    }
  }

  test("a monitor writes its object, as a lock call does, unless the object is fresh") {
    summarised() { (world, summaries) =>
      def at(name: String, descriptor: String = "()V") = of(world, summaries, ref("Alloc", name, descriptor))
      assertEquals(at("locking").effects.map(effect => (effect.kind, effect.onThis)), Set((EffectKind.Writes, true)))
      assertEquals(at("blockThis").effects.map(effect => (effect.kind, effect.onThis)), Set((EffectKind.Writes, true)))
      assertEquals(kinds(at("blockOther", "(Ljava/lang/Object;)V")), Set(EffectKind.Writes))
      assert(isPure(at("blockFresh")))
    }
  }

  test("the origins of the receivers are read from the class file") {
    val file = built.head
    assertEquals(traced(file, "fixture/Alloc", "local").holders.distinct, Vector(Origin.Fresh))
    assertEquals(traced(file, "fixture/Alloc", "param").holders, Vector(Origin.Other))
    assertEquals(traced(file, "fixture/Box", "set").holders, Vector(Origin.This))
  }

  test("a virtual call has the effects of every override the program instantiates") {
    val total  = Reference("fixture/Shapes", "total", "(Lfixture/Shape;)I")
    val square = ref("Shapes", "square", "()Lfixture/Shape;")
    val dirty  = ref("Shapes", "dirty", "()Lfixture/Shape;")
    summarised(Seq(total, square), rooted = _ => false) { (world, summaries) =>
      assert(isPure(of(world, summaries, total)))
    }
    summarised(Seq(total, square, dirty), rooted = _ => false) { (world, summaries) =>
      val found = of(world, summaries, total)
      assertEquals(kinds(found), Set(EffectKind.Writes, EffectKind.Reads))
      assertEquals(found.certain.map(_.kind), Set(EffectKind.Unknown))
    }
  }

  test("an interface call reaches the lambdas the program creates") {
    summarised() { (world, summaries) =>
      assert(kinds(of(world, summaries, ref("Alloc", "runIt", "(Ljava/lang/Runnable;)V"))).contains(EffectKind.Writes))
      assert(isPure(of(world, summaries, ref("Alloc", "lambda", "()Ljava/lang/Runnable;"))))
      assert(isPure(of(world, summaries, ref("Alloc", "quiet", "()Ljava/lang/Runnable;"))))
    }
  }

  test("mutual recursion settles at the union of the effects of the cycle") {
    summarised() { (world, summaries) =>
      val even = of(world, summaries, ref("Cycle", "even", "(I)Z"))
      val odd  = of(world, summaries, ref("Cycle", "odd", "(I)Z"))
      assertEquals(even.effects.map(_.cause), odd.effects.map(_.cause))
      assertEquals(kinds(even), Set(EffectKind.Writes, EffectKind.Reads))
      val fact = of(world, summaries, ref("Cycle", "fact", "(I)I"))
      assert(isPure(fact))
      assert(!fact.bounded && !even.bounded && !odd.bounded)
    }
  }

  test("an axiom is the authority over the body, in both directions") {
    summarised() { (world, summaries) =>
      val quiet = of(world, summaries, Reference("org/slf4j/Quiet", "info", "(Ljava/lang/String;)I"))
      assertEquals(kinds(quiet), Set(EffectKind.Output))
      assertEquals(kinds(of(world, summaries, ref("Authority", "logs"))), Set(EffectKind.Output))
      val loud = of(world, summaries, Reference("scala/runtime/Loud", "bump", "()I"))
      assert(isPure(loud))
      assert(isPure(of(world, summaries, ref("Authority", "loud"))))
      val logs = of(world, summaries, ref("Authority", "logs"))
      assert(logs.effects.forall(!_.assumed))
      assertEquals(logs.certain, logs.effects)
    }
  }

  test("an axiom keyed by a class that has a companion object covers the methods of the object") {
    summarised() { (world, summaries) =>
      val shown = of(world, summaries, Reference("scala/Predef$", "println", "(Ljava/lang/Object;)V"))
      assertEquals(kinds(shown), Set(EffectKind.Output))
    }
  }

  test("an axiom covers a method but not a constructor or an initialiser, whose bodies speak for themselves") {
    summarised() { (world, summaries) =>
      assertEquals(
        kinds(of(world, summaries, Reference("scala/runtime/Boot", "<clinit>", "()V"))),
        Set(EffectKind.Input)
      )
      assertEquals(
        kinds(of(world, summaries, ref("Counted", "<init>", "()V"))),
        Set(EffectKind.Writes, EffectKind.Reads)
      )
      assertEquals(
        kinds(of(world, summaries, Reference("scala/runtime/Boot", "<init>", "()V"))),
        Set(EffectKind.Writes, EffectKind.Reads)
      )
      assertEquals(
        kinds(of(world, summaries, ref("Counted", "make", "()Lfixture/Counted;"))),
        Set(EffectKind.Writes, EffectKind.Reads)
      )
    }
  }

  test("a callee's effects reach the caller as the receiver allows: kept for a parameter, hidden for a fresh object") {
    summarised() { (world, summaries) =>
      def at(name: String, descriptor: String = "()V") = of(world, summaries, ref("Cell", name, descriptor))
      def shapes(summary: BytecodeEffects.Behaviour)   = summary.effects.map(effect => (effect.kind, effect.onThis))
      assertEquals(kinds(at("freshTick")), Set(EffectKind.Writes, EffectKind.Reads))
      assertEquals(kinds(at("paramTick", "(Lfixture/Cell;)V")), Set(EffectKind.Writes, EffectKind.Reads))
      assertEquals(
        shapes(at("both")),
        Set((EffectKind.Writes, true), (EffectKind.Writes, false), (EffectKind.Reads, false))
      )
      assertEquals(shapes(at("freshBoth")), Set((EffectKind.Writes, false), (EffectKind.Reads, false)))
      assertEquals(
        shapes(at("paramBoth", "(Lfixture/Cell;)V")),
        Set((EffectKind.Writes, false), (EffectKind.Reads, false))
      )
    }
  }

  test("a constructor that calls its own setter has no effect on the object it builds") {
    summarised() { (world, summaries) =>
      assert(of(world, summaries, ref("Cell", "<init>", "(I)V")).effects.isEmpty)
      assert(isPure(of(world, summaries, ref("Cell", "build", "(I)Lfixture/Cell;"))))
    }
  }

  test("a value that is a fresh object on one path and a parameter on another is not fresh, and on both paths is") {
    summarised() { (world, summaries) =>
      def at(name: String, descriptor: String) = of(world, summaries, ref("Cell", name, descriptor))
      assertEquals(kinds(at("maybe", "(ZLfixture/Cell;)V")), Set(EffectKind.Writes))
      assertEquals(kinds(at("maybeSwapped", "(ZLfixture/Cell;)V")), Set(EffectKind.Writes))
      assert(isPure(at("bothFresh", "(Z)V")))
    }
  }

  test("a static synchronized method locks nothing the caller holds") {
    summarised() { (world, summaries) =>
      val locked = of(world, summaries, ref("Cell", "locked", "()V"))
      assertEquals(locked.effects.map(effect => (effect.kind, effect.onThis)), Set((EffectKind.Writes, false)))
    }
  }

  test("a throw is not an effect only some implementations have, as the call cannot choose to throw") {
    summarised() { (world, summaries) =>
      val run    = of(world, summaries, ref("Jobs", "run", "(Lfixture/Job;)I"))
      val thrown = run.effects.filter(_.kind == EffectKind.Throws)
      assert(thrown.nonEmpty, run.effects.toString)
      assert(thrown.forall(!_.assumed))
    }
  }

  test("methods of classes whose names differ only by a trailing dollar share a key, and a loop in either bounds both") {
    summarised() { (world, summaries) =>
      val key = world.key(Reference("fixture/Twin", "m", "([I)I"))
      assertEquals(key, world.key(Reference("fixture/Twin$", "m", "([I)I")))
      assert(!summaries.get(key).getOrElse(fail("no summary")).bounded)
      assertEquals(summaries.callee(key), None)
    }
  }

  test("a method is a comparison or an equality contract by its class and signature") {
    summarised() { (_, summaries) =>
      val roles = summaries.owned((_, _) => true).map(entry =>
        (entry.method.owner, entry.method.name, entry.method.descriptor) -> entry.role
      ).toMap
      def role(owner: String, name: String, descriptor: String) = roles((s"fixture/$owner", name, descriptor))
      assertEquals(role("Ranked", "compareTo", "(Lfixture/Ranked;)I"), Some("compareTo"))
      assertEquals(role("Ranked", "compareTo", "(Ljava/lang/Object;)I"), Some("compareTo"))
      assertEquals(role("Ranked", "equals", "(Ljava/lang/Object;)Z"), Some("equals"))
      assertEquals(role("Ranked", "hashCode", "()I"), Some("hashCode"))
      assertEquals(role("Ranked", "hashCode", "(I)I"), None)
      assertEquals(role("Ordering", "compare", "(Lfixture/Ranked;Lfixture/Ranked;)I"), Some("compare"))
      assertEquals(role("Plain", "compare", "(II)I"), None)
      assertEquals(role("Plain", "compareTo", "(Lfixture/Plain;)I"), None)
    }
  }

  test("a nested class has a summary when its outer class is not on the class path") {
    summarised(absent = Set("fixture/Holder")) { (_, summaries) =>
      assert(summaries.owned((_, _) => true).exists(_.method.owner == "fixture/Holder$Nested"))
    }
  }

  test("the reads of a static field of a class the world lacks are an effect, and its initialiser is not guessed at") {
    summarised(absent = Set("fixture/GoneField")) { (world, summaries) =>
      assertEquals(kinds(of(world, summaries, ref("Peeks", "absent"))), Set(EffectKind.Reads))
    }
  }

  test("a static field access runs the initialiser of the class that holds it") {
    summarised() { (world, summaries) =>
      assertEquals(kinds(of(world, summaries, ref("Peeks", "created"))), Set(EffectKind.Reads, EffectKind.Input))
    }
  }

  test("every initialiser the program can run is a method, including those of interfaces nothing else reaches") {
    val konst = ref("Peeks", "konst", "()Ljava/lang/Object;")
    summarised(Seq(konst), rooted = _ => false) { (_, summaries) =>
      val owned = summaries.owned((_, _) => true).map(_.method)
      assert(owned.contains(Reference("fixture/Konst", "<clinit>", "()V")), owned.toString)
    }
  }

  test("a call to a missing class on a path that only throws leaves the method bounded, and one that returns does not") {
    summarised(absent = Set("fixture/Gone")) { (world, summaries) =>
      assert(of(world, summaries, ref("Missing", "guarded", "(I)I")).bounded)
      assert(!of(world, summaries, ref("Missing", "call")).bounded)
    }
  }

  test("a call through a super-interface, a bridge or a marked lambda reaches the method the lambda binds") {
    def pulls(name: String, descriptor: String) = Reference("fixture/Pulls", name, descriptor)
    val roots                                   = Seq(
      pulls("bound", "(Lfixture/Name;)Lfixture/Text;"),
      pulls("tagged", "(Lfixture/Name;)Ljava/util/function/Supplier;"),
      pulls("name", "()Lfixture/Name;"),
      pulls("pull", "(Lfixture/Source;)Ljava/lang/Object;"),
      pulls("pullText", "(Lfixture/Text;)Ljava/lang/String;"),
      pulls("pullSupplier", "(Ljava/util/function/Supplier;)Ljava/lang/String;")
    )
    summarised(roots, rooted = _ => false) { (world, summaries) =>
      roots.drop(3).foreach { root =>
        val found = of(world, summaries, root)
        assertEquals(found.effects.map(_.cause), Set("reads text"), root.name)
        assert(found.effects.forall(!_.assumed), root.name)
      }
    }
  }

  test("JDK methods are classified by the axioms where they cover them, and by their bytecode elsewhere") {
    val abs    = Reference("java/lang/Math", "abs", "(I)I")
    val parse  = Reference("java/lang/Integer", "parseInt", "(Ljava/lang/String;)I")
    val millis = Reference("java/lang/System", "currentTimeMillis", "()J")
    val add    = Reference("java/util/ArrayList", "add", "(Ljava/lang/Object;)Z")
    summarised(Seq(abs, parse, millis, add)) { (world, summaries) =>
      assert(isPure(of(world, summaries, abs)) && of(world, summaries, abs).bounded)
      assertEquals(kinds(of(world, summaries, parse)), Set(EffectKind.Throws))
      assertEquals(kinds(of(world, summaries, millis)), Set(EffectKind.Random))
      assertEquals(kinds(of(world, summaries, add)), Set(EffectKind.Writes))
      assert(of(world, summaries, add).effects.forall(_.onThis))
      assertEquals(kinds(of(world, summaries, ref("Jdk", "add", "(Ljava/util/ArrayList;)Z"))), Set(EffectKind.Writes))
      assertEquals(
        kinds(of(world, summaries, ref("Jdk", "get", "(Ljava/util/ArrayList;)Ljava/lang/Object;"))),
        Set(EffectKind.Reads)
      )
    }
  }

  test("a JDK method no axiom covers takes the effects of its body") {
    summarised() { (world, summaries) =>
      val listed = of(world, summaries, ref("Jdk", "listed", "([Ljava/lang/String;)Ljava/util/List;"))
      assert(isPure(listed), listed.effects.toString)
      assert(listed.effects.forall(_.kind == EffectKind.Throws))
      val random = of(world, summaries, ref("Jdk", "random", "()Ljava/lang/Object;"))
      assert(kinds(random).contains(EffectKind.Unknown), random.effects.toString)
    }
  }

  test("a JDK cache is the axiom's to judge, and the same cache in a class no axiom covers is a write") {
    val hash   = Reference("java/lang/String", "hashCode", "()I")
    val locale = Reference("java/util/Locale", "getDefault", "()Ljava/util/Locale;")
    val url    = Reference("java/net/URL", "hashCode", "()I")
    summarised(Seq(hash, locale, url)) { (world, summaries) =>
      assert(isPure(of(world, summaries, hash)))
      assert(world.get("java/lang/String").exists(_.methods.exists(_.name == "hashCode")))
      assertEquals(kinds(of(world, summaries, locale)), Set(EffectKind.Reads))
      assert(!isPure(of(world, summaries, url)))
    }
  }

  test("effects name what the method does") {
    summarised() { (world, summaries) =>
      def causes(owner: String, name: String, descriptor: String = "()I") =
        of(world, summaries, ref(owner, name, descriptor)).effects.map(_.cause)
      assertEquals(causes("Alloc", "locking", "()V"), Set("locks a monitor"))
      assertEquals(causes("Alloc", "nat"), Set("is native"))
      assertEquals(causes("Alloc", "store", "([I)V"), Set("writes an array element"))
      assertEquals(causes("Alloc", "load", "([I)I"), Set("reads an array element"))
      assertEquals(causes("Alloc", "bump", "()V"), Set("writes counter", "reads counter"))
      assertEquals(causes("Box", "set", "(I)V"), Set("writes value"))
      assertEquals(causes("Box", "get"), Set("reads value"))
      assert(causes("Bounds", "failing", "([II)I").contains("throws"))
      assert(causes("Dyn", "show", "(Lfixture/Point;)Ljava/lang/String;").exists(_.startsWith("runs dynamic code of")))
    }
  }

  test("an effect only some implementations perform is unknown when it must hold of every run") {
    summarised() { (world, summaries) =>
      val total = of(world, summaries, ref("Shapes", "total", "(Lfixture/Shape;)I"))
      assert(total.effects.exists(_.assumed))
      assert(total.certain.forall(!_.assumed))
      assert(total.certain.exists { effect =>
        effect.kind == EffectKind.Unknown && effect.cause.endsWith(" in some implementation")
      })
    }
  }

  test("a method that runs in constant time stands for its call in a cost, and no summary stands for nothing") {
    summarised() { (world, summaries) =>
      assert(BytecodeEffects.none.isEmpty && !summaries.isEmpty)
      assert(summaries.callee(world.key(ref("Bounds", "constant", "(I)I"))).isDefined)
      assertEquals(summaries.callee(world.key(ref("Bounds", "looped", "([I)I"))), None)
      assertEquals(summaries.callee("fixture.Nothing.at(I)I"), None)
      assert(summaries.require(world.key(ref("Bounds", "constant", "(I)I"))))
      assert(!summaries.require("fixture.Nothing.at(I)I"))
      assert(!BytecodeEffects.none.require(world.key(ref("Bounds", "constant", "(I)I"))))
      assertEquals(BytecodeEffects.none.get(world.key(ref("Bounds", "constant", "(I)I"))), None)
      assert(BytecodeEffects.none.owned((_, _) => true).isEmpty)
    }
  }

  test("a call through a functional interface reaches the method a lambda binds") {
    summarised() { (world, summaries) =>
      val call = of(world, summaries, ref("Handles", "call", "(Ljava/util/function/Supplier;)I"))
      assert(kinds(call).contains(EffectKind.Reads))
    }
  }

  test("a cycle of three methods and what calls it are unbounded, and what only calls a leaf is bounded") {
    summarised() { (world, summaries) =>
      def bounded(name: String) = of(world, summaries, ref("Cycle", name, "(I)I")).bounded
      assertEquals(Seq("ping", "pong", "pang", "entry", "fact").filter(bounded(_)), Seq.empty[String])
      assertEquals(Seq("leaf", "after").filterNot(bounded(_)), Seq.empty[String])
    }
  }

  test("a write into a static array is no effect in the initialiser of its own class, and an effect anywhere else") {
    summarised() { (world, summaries) =>
      assertEquals(kinds(of(world, summaries, ref("Table", "poke", "(I)V"))), Set(EffectKind.Writes))
      assertEquals(kinds(of(world, summaries, ref("Poker", "<clinit>", "()V"))), Set(EffectKind.Writes))
    }
  }

  test("a callee the class path lacks is unknown") {
    summarised(absent = Set("fixture/Gone")) { (world, summaries) =>
      assertEquals(kinds(of(world, summaries, ref("Missing", "call"))), Set(EffectKind.Unknown))
      assert(of(world, summaries, ref("Missing", "call")).effects.exists { effect =>
        effect.cause.startsWith("calls ") && effect.cause.endsWith("Gone.m")
      })
      assert(!of(world, summaries, ref("Missing", "call")).bounded)
    }
  }

  test("an invokedynamic that is not a lambda or a string concatenation is unknown") {
    summarised() { (world, summaries) =>
      val shown = of(world, summaries, Reference("fixture/Point", "toString", "()Ljava/lang/String;"))
      assert(kinds(shown).contains(EffectKind.Unknown), shown.effects.toString)
      assert(kinds(of(
        world,
        summaries,
        ref("Dyn", "show", "(Lfixture/Point;)Ljava/lang/String;")
      )).contains(EffectKind.Unknown))
      assert(isPure(of(
        world,
        summaries,
        ref("Bounds", "joined", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;")
      )))
    }
  }

  test("a reflective call is not taken for pure") {
    summarised() { (world, summaries) =>
      val call =
        of(world, summaries, ref("Refl", "call", "(Ljava/lang/reflect/Method;Ljava/lang/Object;)Ljava/lang/Object;"))
      assert(!isPure(call), call.effects.toString)
    }
  }

  test("a lazily cached field is a write on the receiver, which only a fresh receiver hides") {
    summarised() { (world, summaries) =>
      val hash = of(world, summaries, ref("Cached", "hashCode"))
      assertEquals(
        hash.effects.map(effect => (effect.kind, effect.onThis)),
        Set((EffectKind.Writes, true), (EffectKind.Reads, true))
      )
      assertEquals(
        kinds(of(world, summaries, ref("Cached", "viaParam", "(Lfixture/Cached;)I"))),
        Set(EffectKind.Writes, EffectKind.Reads)
      )
      assert(isPure(of(world, summaries, ref("Cached", "viaFresh"))))
    }
  }

  test("an initialiser's effects attach to the first access, except writes to its own class's statics") {
    summarised() { (world, summaries) =>
      val home = of(world, summaries, ref("Users", "home", "()Ljava/lang/String;"))
      assertEquals(kinds(home), Set(EffectKind.Input), home.effects.toString)
      assert(of(world, summaries, ref("Registry", "home", "()Ljava/lang/String;")).effects.isEmpty)
      assert(of(world, summaries, ref("Registry", "<clinit>", "()V")).effects.exists(_.kind == EffectKind.Input))
      assert(of(world, summaries, ref("Table", "<clinit>", "()V")).effects.isEmpty)
      assert(isPure(of(world, summaries, ref("Users", "made", "()Ljava/lang/Object;"))))
    }
  }

  test("a method with no loop, recursion, allocation or unbounded callee runs in constant time") {
    summarised() { (world, summaries) =>
      def at(name: String, descriptor: String) = of(world, summaries, ref("Bounds", name, descriptor))
      assert(at("constant", "(I)I").bounded)
      assert(at("callsConstant", "(I)I").bounded)
      assert(!at("looped", "([I)I").bounded)
      assert(!at("callsLoop", "([I)I").bounded)
      assert(!at("allocates", "(I)I").bounded)
      assert(!at("recursive", "(I)I").bounded)
      assert(!at("joined", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;").bounded)
    }
  }

  test("a loop on a path that only throws does not bound the method") {
    summarised() { (world, summaries) =>
      val failing = of(world, summaries, ref("Bounds", "failing", "([II)I"))
      assert(failing.bounded)
      assert(kinds(failing).contains(EffectKind.Throws))
    }
  }

  test("ArrayList.get is constant and List.contains is not") {
    val get      = Reference("java/util/ArrayList", "get", "(I)Ljava/lang/Object;")
    val contains = Reference("java/util/ArrayList", "contains", "(Ljava/lang/Object;)Z")
    summarised(Seq(get, contains)) { (world, summaries) =>
      assert(of(world, summaries, get).bounded)
      assert(!of(world, summaries, contains).bounded)
      assert(!of(world, summaries, ref("Lists", "contains", "(Ljava/util/List;Ljava/lang/Object;)Z")).bounded)
      assert(summaries.callee(world.key(get)).exists(_.bound == Charges.Bound(Polynomial.One, Polynomial.Zero)))
      assertEquals(summaries.callee(world.key(contains)), None)
    }
  }

  private val scalaApi = """package fixture

object ScalaApi {
  def run(): Int = 1
  def lazyOne: () => Int = () => 2
}
"""

  private val javaSide = """package fixture;

public class JavaSide {
  public static int callBack() { return ScalaApi.run(); }
  public static int own() { return 3; }
  public static Object mod() { return ScalaApi$.MODULE$; }
  public static int mixed(Mixed m) { return m.run(); }
  public static Mixed javaMixed() { return new JavaMixed(); }
  public static Mixed scalaMixed() { return new ScalaMixed(); }
}
"""

  private val mixed = """package fixture;

public interface Mixed {
  int run();
}
"""

  private val javaMixed = """package fixture;

public class JavaMixed implements Mixed {
  public int run() { return 2; }
}
"""

  private val scalaMixed = """package fixture

class ScalaMixed extends Mixed {
  def run(): Int = 4
}

class Dual {
  def one: Int = 1
}

class Bare {
  def one: Int = 1
}
"""

  private val dual = """package fixture;

public class Dual$ {
  public static int three() { return 3; }
}
"""

  test("a call through the method handle of an interface method reaches the implementations") {
    def sizing(name: String, descriptor: String) = Reference("fixture/Sizing", name, descriptor)
    val roots                                    = Seq(
      sizing("sizer", "(Lfixture/Sized;)Ljava/util/function/Supplier;"),
      sizing("fixed", "()Lfixture/Sized;"),
      sizing("pull", "(Ljava/util/function/Supplier;)I")
    )
    summarised(roots, rooted = _ => false) { (world, summaries) =>
      val found = of(world, summaries, roots(2))
      assertEquals(found.effects.map(_.cause), Set("reads n"))
      assert(found.effects.forall(!_.assumed))
    }
  }

  // scalafix:off DisableSyntax.null
  @SuppressWarnings(Array("bonsai:long-arguments", "bonsai:same-type-arguments"))
  private def legacy(directory: Path): Path = {
    def emit(name: String, source: Option[String]): Unit = {
      val writer = ClassWriter(0)
      writer.visit(Opcodes.V17, Opcodes.ACC_PUBLIC, s"fixture/$name", null, "java/lang/Object", null)
      source.foreach(writer.visitSource(_, null))
      val method = writer.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "one", "()I", null, null)
      method.visitCode()
      method.visitInsn(Opcodes.ICONST_1)
      method.visitInsn(Opcodes.IRETURN)
      method.visitMaxs(1, 0)
      method.visitEnd()
      writer.visitEnd()
      Files.write(
        Files.createDirectories(directory.resolve("fixture")).resolve(s"$name.class"),
        writer.toByteArray
      ): Unit
    }
    emit("Legacy", Some("Legacy.scala"))
    emit("Bare$", None)
    directory
  }

  // scalafix:on DisableSyntax.null

  test("classes with TASTy get no summary and are unknown to the bytecode that calls them") {
    val directory  = Scratch.directory("bonsai-bytecode-tasty-")
    val interfaces = Files.createDirectories(directory.resolve("interfaces"))
    write(directory.resolve("src"), Seq("fixture/Mixed.java" -> mixed))
    javac(Seq(directory.resolve("src/fixture/Mixed.java")), interfaces, classpath): Unit
    val scala =
      compile(
        directory.resolve("scala"),
        Seq("ScalaApi.scala" -> scalaApi, "ScalaMixed.scala" -> scalaMixed),
        Vector(interfaces)
      )
    val classes     = Files.createDirectories(directory.resolve("java"))
    val javaSources =
      Seq("fixture/JavaSide.java" -> javaSide, "fixture/JavaMixed.java" -> javaMixed, "fixture/Dual$.java" -> dual)
    write(directory.resolve("src"), javaSources)
    javac(
      javaSources.map((name, _) => directory.resolve("src").resolve(name)),
      classes,
      classpath ++ Vector(interfaces, scala)
    ): Unit
    summarised(outputs = Vector(classes, interfaces, scala, legacy(directory.resolve("legacy")))) {
      (world, summaries) =>
        val owners = summaries.owned((_, _) => true).map(_.method.owner).toSet
        assert(!owners.exists(_.startsWith("fixture/ScalaApi")), owners.toString)
        assert(!owners.contains("fixture/ScalaMixed") && !owners.contains("fixture/Dual"), owners.toString)
        assert(owners.contains("fixture/JavaSide") && owners.contains("fixture/JavaMixed"))
        assert(owners.contains("fixture/Dual$"), owners.toString)
        assert(owners.contains("fixture/Legacy") && owners.contains("fixture/Bare$"), owners.toString)
        assert(world.get("fixture/ScalaApi").exists(_.tasty))
        assert(world.get("fixture/ScalaApi$").exists(!_.tasty))
        assertEquals(kinds(of(world, summaries, ref("JavaSide", "callBack"))), Set(EffectKind.Unknown))
        assert(isPure(of(world, summaries, ref("JavaSide", "own"))))
        assert(kinds(of(world, summaries, ref("JavaSide", "mixed", "(Lfixture/Mixed;)I"))).contains(EffectKind.Unknown))
        val mod = of(world, summaries, ref("JavaSide", "mod", "()Ljava/lang/Object;"))
        assertEquals(kinds(mod), Set(EffectKind.Unknown))
        assert(mod.effects.forall(_.cause.endsWith("<clinit>")), mod.effects.toString)
        assert(mod.bounded)
    }
  }

  test("a Java-only build has a purity model of its own methods") {
    val result = javaOnly(
      Seq(
        "demo/Counter.java" -> """package demo;

public class Counter {
  private int n;
  private static int total;

  public int next() { return ++n; }
  public static int sum(int a, int b) { return a + b; }
  public static void record() { total++; }
  public static Object fresh() { Counter c = new Counter(); c.next(); return c; }
  public boolean equals(Object o) { return o == this; }
  public int hashCode() { return n; }
  public static void main(String[] args) {
    Counter counter = new Counter();
    System.out.println(sum(counter.next(), fresh().hashCode()));
    record();
  }
}
"""
      )
    )
    val methods = result.purity.methods
    assert(methods.nonEmpty)
    def verdict(name: String) =
      methods.collectFirst { case (key, method) if key.contains(s".$name(") => method.verdict }
    assertEquals(verdict("sum"), Some(Purity.Verdict.Pure))
    assertEquals(verdict("fresh"), Some(Purity.Verdict.Pure))
    assert(verdict("record").exists(_ != Purity.Verdict.Pure))
    assert(verdict("next").exists(_ != Purity.Verdict.Pure))
    assert(methods.exists((key, method) => key.contains(".hashCode(") && method.role.contains("hashCode")))
    assert(Purity.lock(result.purity, result.graph.nodes).contains("demo.Counter.sum(II)I"))
  }

  private def javaOnly(files: Seq[(String, String)]): Extraction.Result =
    Scratch.using("bonsai-java-only-") { directory =>
      write(directory.resolve("src"), files)
      val classes = Files.createDirectories(directory.resolve("classes"))
      val model   = directory.resolve("java-model.tsv")
      JavaModel.write(javac(files.map((name, _) => directory.resolve("src").resolve(name)), classes, classpath), model)
      val output   = Output("j", Output.Scope.Main, classes)
      val manifest =
        Manifest(
          Vector(output),
          classpaths,
          Set.empty,
          models = Vector(Output("j", Output.Scope.Main, model)),
          jdk = targetJdk
        )
      Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
    }

  private val callers = """package fixture

import java.util.concurrent.ThreadLocalRandom

object Callers {
  def listed(a: Array[String]): java.util.List[String] = java.util.Arrays.asList(a*)
  def random(): Object = ThreadLocalRandom.current()
  def main(args: Array[String]): Unit = println(listed(args))
}
"""

  private lazy val scalaBuild: Purity.Model =
    Scratch.using("bonsai-scala-purity-") { directory =>
      val classes  = compile(directory.resolve("main"), Seq("Callers.scala" -> callers), Vector.empty)
      val output   = Output("m", Output.Scope.Main, classes)
      val manifest = Manifest(
        Vector(output),
        Map("m" -> classpath),
        Set.empty,
        sources = Vector(Output("m", Output.Scope.Main, directory.resolve("main/Callers.scala"))),
        jdk = targetJdk
      )
      Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), _.purity)
    }

  test("a module of Java sources alone resolves for the Scala modules that use it") {
    Scratch.using("bonsai-java-module-") { directory =>
      val java = directory.resolve("java")
      write(java, Seq("fixture/Hook.java" -> "package fixture; public interface Hook { void fire(); }"))
      val javaClasses = Files.createDirectories(java.resolve("classes"))
      javac(Seq(java.resolve("fixture/Hook.java")), javaClasses, classpath): Unit
      val source   = """package fixture
class Impl extends Hook { def fire(): Unit = println("fired") }
object Uses {
  def run(hook: Hook): Unit = hook.fire()
  def main(args: Array[String]): Unit = run(() => println(args.length))
}
"""
      val classes  = compile(directory.resolve("scala"), Seq("Uses.scala" -> source), Vector(javaClasses))
      val manifest = Manifest(
        Vector(Output("j", Output.Scope.Main, javaClasses), Output("m", Output.Scope.Main, classes)),
        Map("m" -> classpath),
        Set.empty,
        sources = Vector(Output("m", Output.Scope.Main, directory.resolve("scala/Uses.scala"))),
        jdk = targetJdk
      )
      val result = Extraction.read(manifest).fold(errors => fail(errors.mkString("\n")), identity)
      assert(result.problems.isEmpty, result.problems.mkString("\n"))
      assert(result.notes.isEmpty, result.notes.mkString("\n"))
    }
  }

  test("a Java library call that was unknown is classified, and one that must stay unknown stays") {
    def verdict(model: Purity.Model, name: String) = model.methods.collectFirst {
      case (key, method) if key.startsWith(s"fixture.Callers.$name") => method.verdict
    }.getOrElse(fail(s"no verdict for $name in ${model.methods.keySet}"))
    assertEquals(verdict(scalaBuild, "listed"), Purity.Verdict.Pure)
    assert(verdict(scalaBuild, "random") != Purity.Verdict.Pure)
    assert(Purity.summary(scalaBuild).head.contains("methods"))
  }
}
