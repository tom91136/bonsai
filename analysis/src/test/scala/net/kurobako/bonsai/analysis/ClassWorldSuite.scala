package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.analysis.ClassFiles.*
import net.kurobako.bonsai.testing.*
import scala.collection.mutable
import scala.util.Using

class ClassWorldSuite extends AnalysisSuite {
  private val worlds = """package fixture;

interface I1 {
  default int m() {
    return 1;
  }
}

interface I2 extends I1 {
  default int m() {
    return 2;
  }
}

interface I3 extends I1 {
}

class C implements I2, I3 {
}

class D extends C {
  public int m() {
    return 3;
  }
}

class Plain implements I1 {
}

abstract class AbstractPlain implements I1 {
}

class Leaf extends AbstractPlain {
}

class Base {
  public int base() {
    return 4;
  }
}

interface Both {
  default int base() {
    return 5;
  }
}

class Sub extends Base implements Both {
}

class Privates {
  private int p() {
    return 6;
  }
}

class PrivateSub extends Privates {
  int p() {
    return 7;
  }
}

class Statics {
  static int s() {
    return 8;
  }
}

class StaticSub extends Statics {
}
"""

  private lazy val classes: Path = {
    val directory = Scratch.directory("bonsai-class-worlds-")
    write(directory.resolve("src"), Seq("Worlds.java" -> worlds))
    val output = Files.createDirectories(directory.resolve("classes"))
    javac(Seq(directory.resolve("src").resolve("Worlds.java")), output, classpath): Unit
    output
  }

  private def load(name: String): Option[Array[Byte]] = {
    val file = classes.resolve(s"$name.class")
    if (Files.exists(file)) Some(Files.readAllBytes(file))
    else
      Option(ClassLoader.getSystemResourceAsStream(s"$name.class")).map(stream =>
        Using.resource(stream)(_.readAllBytes())
      )
  }

  private def fresh: ClassWorld = ClassWorld(load(_))

  private def withMajor(bytes: Array[Byte], major: Int): Array[Byte] =
    bytes.updated(6, (major >> 8).toByte).updated(7, major.toByte)

  test("selects the override declared by the receiver class") {
    assertEquals(fresh.select("fixture/D", Reference("fixture/I1", "m", "()I")).map(_.owner), Some("fixture/D"))
  }

  test("selects the most specific default of a diamond") {
    assertEquals(fresh.select("fixture/C", Reference("fixture/I1", "m", "()I")).map(_.owner), Some("fixture/I2"))
  }

  test("selects the default of the interface that declares it") {
    val world = fresh
    assertEquals(world.select("fixture/Plain", Reference("fixture/I1", "m", "()I")).map(_.owner), Some("fixture/I1"))
    assertEquals(world.select("fixture/Leaf", Reference("fixture/I1", "m", "()I")).map(_.owner), Some("fixture/I1"))
  }

  test("prefers an inherited class method to a default method") {
    assertEquals(
      fresh.select("fixture/Sub", Reference("fixture/Sub", "base", "()I")).map(_.owner),
      Some("fixture/Base")
    )
  }

  test("selects private and static methods without dispatch") {
    val world = fresh
    assertEquals(
      world.select("fixture/PrivateSub", Reference("fixture/Privates", "p", "()I")).map(_.owner),
      Some("fixture/Privates")
    )
    assertEquals(
      world.select("fixture/StaticSub", Reference("fixture/Statics", "s", "()I")).map(_.owner),
      Some("fixture/Statics")
    )
  }

  test("resolves signature polymorphic methods by name") {
    assertEquals(
      fresh.resolve(Reference("java/lang/invoke/MethodHandle", "invokeExact", "(II)V")).map(_.owner),
      Some("java/lang/invoke/MethodHandle")
    )
  }

  test("selects the Object method for an array receiver") {
    assertEquals(
      fresh.select("[I", Reference("[I", "clone", "()Ljava/lang/Object;")).map(_.owner),
      Some("java/lang/Object")
    )
  }

  test("lists the ancestors of a class, superclasses first") {
    assertEquals(
      fresh.ancestors("fixture/D"),
      Vector("fixture/C", "java/lang/Object", "fixture/I2", "fixture/I3", "fixture/I1")
    )
  }

  test("reads each class once") {
    val reads = mutable.ArrayBuffer.empty[String]
    val world = ClassWorld { name =>
      reads.append(name)
      load(name)
    }
    assertEquals(world.ancestors("fixture/D"), world.ancestors("fixture/D"))
    assertEquals(reads.toVector.distinct, reads.toVector)
  }

  test("reports missing and unreadable classes") {
    val patched = load("fixture/Plain").map(withMajor(_, 72))
    val world   = ClassWorld(name => if (name == "fixture/Plain") patched else load(name))
    assertEquals(world.get("fixture/Nope"), None)
    assertEquals(world.get("fixture/Plain"), None)
    assertEquals(world.missing, Set("fixture/Nope"))
    assertEquals(world.unreadable.keySet, Set("fixture/Plain"))
    assert(world.unreadable("fixture/Plain").contains("72"), world.unreadable.toString)
  }
}
