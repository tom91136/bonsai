package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.analysis.ClassFiles.*
import net.kurobako.bonsai.testing.*
import org.objectweb.asm.Opcodes

class ClassFilesSuite extends AnalysisSuite {
  private val kinds = """package fixture;

import java.util.List;

public class Kinds {
  int value;
  static int total;

  native int nativeCall(int x);

  int length(String s) {
    return s.length();
  }

  int size(List<String> items) {
    return items.size();
  }

  int abs(int x) {
    return Math.abs(x);
  }

  int hash() {
    return super.hashCode();
  }

  Object make() {
    return new StringBuilder();
  }

  void lock(Object o) {
    synchronized (o) {
    }
  }

  String concat(int x) {
    return "a" + x;
  }

  int[] copy(int[] a) {
    return a.clone();
  }

  int read() {
    return value;
  }

  void write(int x) {
    total = x;
  }
}
"""

  private val lambdas = """package fixture;

import java.util.function.Supplier;

public class Lambdas {
  Supplier<Lambdas> make() {
    return Lambdas::new;
  }

  Runnable noop() {
    return () -> {};
  }
}
"""

  private val shapes = """package fixture;

public interface Shapes {
  double area();

  default String label() {
    return "shape";
  }

  static int count() {
    return 0;
  }
}
"""

  private val scalas = """package fixture

class Scalas {
  def increment(xs: List[Int]): List[Int] = xs.map(x => x + 1)
}
"""

  private lazy val classes: Path = {
    val directory = Scratch.directory("bonsai-class-files-")
    val sources   = Seq("Kinds.java" -> kinds, "Lambdas.java" -> lambdas, "Shapes.java" -> shapes)
    write(directory.resolve("src"), sources)
    val output = Files.createDirectories(directory.resolve("classes"))
    javac(sources.map((name, _) => directory.resolve("src").resolve(name)), output, classpath): Unit
    output
  }

  private def classBytes(name: String): Array[Byte] = Files.readAllBytes(classes.resolve(s"$name.class"))

  private def parse(bytes: Array[Byte]): ClassFile = ClassFiles.read(bytes).fold(error => fail(error), identity)

  private def methodOf(file: ClassFile, name: String): Method =
    file.methods.find(_.name == name).getOrElse(fail(s"no method $name in ${file.name}"))

  private def lineOf(source: String, fragment: String): Int = source.linesIterator.indexWhere(_.contains(fragment)) + 1

  private def withMajor(bytes: Array[Byte], major: Int): Array[Byte] =
    bytes.updated(6, (major >> 8).toByte).updated(7, major.toByte)

  private def kindsFile: ClassFile = parse(classBytes("fixture/Kinds"))

  test("reads invocations with their kinds and lines") {
    val file = kindsFile
    assert(
      methodOf(file, "length").invocations.contains(
        Invocation(Kind.Virtual, "java/lang/String", "length", "()I", lineOf(kinds, "return s.length()"))
      )
    )
    assert(
      methodOf(file, "size").invocations.contains(
        Invocation(Kind.Interface, "java/util/List", "size", "()I", lineOf(kinds, "items.size()"))
      )
    )
    assert(
      methodOf(file, "abs").invocations.contains(
        Invocation(Kind.Static, "java/lang/Math", "abs", "(I)I", lineOf(kinds, "Math.abs"))
      )
    )
    assert(
      methodOf(file, "hash").invocations.contains(
        Invocation(Kind.Special, "java/lang/Object", "hashCode", "()I", lineOf(kinds, "super.hashCode()"))
      )
    )
  }

  test("reads the line of a method and its field accesses") {
    val file = kindsFile
    assertEquals(methodOf(file, "length").line, lineOf(kinds, "return s.length()"))
    assertEquals(
      methodOf(file, "read").fields,
      Vector(FieldAccess("fixture/Kinds", "value", "I", static = false, write = false))
    )
    assertEquals(
      methodOf(file, "write").fields,
      Vector(FieldAccess("fixture/Kinds", "total", "I", static = true, write = true))
    )
  }

  test("reads monitors and instantiations") {
    val file = kindsFile
    assert(methodOf(file, "lock").monitors)
    assert(!methodOf(file, "hash").monitors)
    assertEquals(methodOf(file, "make").instantiated, Set("java/lang/StringBuilder"))
    assert(
      methodOf(file, "make").invocations.contains(
        Invocation(Kind.Special, "java/lang/StringBuilder", "<init>", "()V", lineOf(kinds, "new StringBuilder"))
      )
    )
  }

  test("reads string concatenation as a dynamic site") {
    val concat = methodOf(kindsFile, "concat")
    assertEquals(
      concat.dynamics.map(site => (site.owner, site.name)),
      Vector(("java/lang/invoke/StringConcatFactory", "makeConcatWithConstants"))
    )
    assert(concat.lambdas.isEmpty)
  }

  test("reads Java lambdas and constructor references") {
    val file = parse(classBytes("fixture/Lambdas"))
    assertEquals(
      methodOf(file, "make").lambdas.map(lambda =>
        (lambda.interface, lambda.sam, lambda.owner, lambda.name, lambda.tag)
      ),
      Vector(("java/util/function/Supplier", "get", "fixture/Lambdas", "<init>", Opcodes.H_NEWINVOKESPECIAL))
    )
    assertEquals(
      methodOf(file, "noop").lambdas.map(lambda => (lambda.interface, lambda.tag)),
      Vector(("java/lang/Runnable", Opcodes.H_INVOKESTATIC))
    )
  }

  test("reads Scala lambdas as lambdas of their class") {
    Scratch.using("bonsai-class-files-") { directory =>
      val output = compile(directory, Seq("Scalas.scala" -> scalas), Vector.empty)
      val file   = parse(Files.readAllBytes(output.resolve("fixture/Scalas.class")))
      assertEquals(
        methodOf(file, "increment").lambdas.map(lambda => (lambda.interface, lambda.sam, lambda.owner, lambda.name)),
        Vector(("scala/runtime/java8/JFunction1$mcII$sp", "apply$mcII$sp", "fixture/Scalas", "increment$$anonfun$1"))
      )
    }
  }

  test("reads access flags of native, abstract and static methods") {
    assert(methodOf(kindsFile, "nativeCall").isNative)
    val shapesFile = parse(classBytes("fixture/Shapes"))
    assert(methodOf(shapesFile, "area").isAbstract)
    assert(!methodOf(shapesFile, "label").isAbstract)
    assert(methodOf(shapesFile, "count").isStatic)
  }

  test("reads array receivers without failing") {
    assertEquals(
      methodOf(kindsFile, "copy").invocations.map(call => (call.kind, call.owner, call.name)),
      Vector((Kind.Virtual, "[I", "clone"))
    )
  }

  test("reads the class header") {
    val file = kindsFile
    assertEquals(file.name, "fixture/Kinds")
    assertEquals(file.superName, Some("java/lang/Object"))
    assertEquals(file.interfaces, Vector.empty[String])
    assertEquals(file.sourceFile, Some("Kinds.java"))
  }

  test("reads a class of major version 71") {
    assertEquals(ClassFiles.read(withMajor(classBytes("fixture/Kinds"), 71)).map(_.version), Right(71))
  }

  test("rejects a class of a newer major version, naming the version") {
    val result = ClassFiles.read(withMajor(classBytes("fixture/Kinds"), 72))
    assert(result.left.exists(_.contains("72")), result.toString)
  }
}
