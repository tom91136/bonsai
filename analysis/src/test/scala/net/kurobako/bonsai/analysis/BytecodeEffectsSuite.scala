package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.analysis.ClassFiles.*
import net.kurobako.bonsai.effects.Kind as EffectKind
import net.kurobako.bonsai.java.Model as JavaModel
import net.kurobako.bonsai.testing.*
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
}
"""

  test("classes with TASTy get no summary and are unknown to the bytecode that calls them") {
    val directory = Scratch.directory("bonsai-bytecode-tasty-")
    val scala     = compile(directory.resolve("scala"), Seq("ScalaApi.scala" -> scalaApi), Vector.empty)
    val classes   = Files.createDirectories(directory.resolve("java"))
    write(directory.resolve("src"), Seq("fixture/JavaSide.java" -> javaSide))
    javac(Seq(directory.resolve("src/fixture/JavaSide.java")), classes, classpath ++ Vector(scala)): Unit
    summarised(outputs = Vector(classes, scala)) { (world, summaries) =>
      val owners = summaries.entries.map(_.method.owner).toSet
      assert(!owners.exists(_.startsWith("fixture/ScalaApi")), owners.toString)
      assert(owners.contains("fixture/JavaSide"))
      assert(world.get("fixture/ScalaApi").exists(_.tasty))
      assert(world.get("fixture/ScalaApi$").exists(!_.tasty))
      assertEquals(kinds(of(world, summaries, ref("JavaSide", "callBack"))), Set(EffectKind.Unknown))
      assert(isPure(of(world, summaries, ref("JavaSide", "own"))))
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

  test("a Java library call that was unknown is classified, and one that must stay unknown stays") {
    def verdict(model: Purity.Model, name: String) = model.methods.collectFirst {
      case (key, method) if key.startsWith(s"fixture.Callers.$name") => method.verdict
    }.getOrElse(fail(s"no verdict for $name in ${model.methods.keySet}"))
    assertEquals(verdict(scalaBuild, "listed"), Purity.Verdict.Pure)
    assert(verdict(scalaBuild, "random") != Purity.Verdict.Pure)
    assert(Purity.summary(scalaBuild).head.contains("methods"))
  }
}
