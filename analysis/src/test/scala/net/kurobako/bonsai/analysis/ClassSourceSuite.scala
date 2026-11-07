package net.kurobako.bonsai.analysis

import java.nio.ByteBuffer
import java.nio.file.Files
import java.nio.file.Path
import munit.FunSuite
import net.kurobako.bonsai.testing.*
import scala.util.Using

class ClassSourceSuite extends FunSuite {
  private def withRoot(name: String)(body: Path => Unit): Unit =
    test(name)(Scratch.using("bonsai-classes-")(body))

  private val javaHome = Path.of(sys.props("java.home"))

  private def write(root: Path, relative: String, content: String): Unit = {
    val file = root.resolve(relative)
    Files.createDirectories(file.getParent)
    Files.writeString(file, content): Unit
  }

  private def jar(file: Path, entries: (String, String)*): Unit = {
    Files.createDirectories(file.getParent)
    Using.resource(java.util.zip.ZipOutputStream(Files.newOutputStream(file))) { zip =>
      entries.foreach { (name, content) =>
        zip.putNextEntry(java.util.zip.ZipEntry(name))
        zip.write(content.getBytes("UTF-8"))
        zip.closeEntry()
      }
    }
  }

  private val multiRelease = "META-INF/MANIFEST.MF" -> "Manifest-Version: 1.0\nMulti-Release: true\n\n"

  private def jdk8(home: Path, entries: (String, String)*): Path = {
    write(home, "release", "JAVA_VERSION=\"1.8.0_402\"\n")
    jar(home.resolve("jre/lib/rt.jar"), entries*)
    home
  }

  private def source(outputs: Vector[Path], classpath: Vector[Path], jdk: Option[Path]): ClassSource =
    ClassSource.of(outputs, classpath, jdk) match {
      case Right(source) => source
      case Left(error)   => fail(error)
    }

  private def lookup(
      jdk: Option[Path],
      outputs: Vector[Path],
      classpath: Vector[Path],
      internal: String
  ): Option[String] =
    Using.resource(source(outputs, classpath, jdk))(_.bytes(internal)).map(String(_, "UTF-8"))

  withRoot("a jdk 8 home reads the rt.jar under jre/lib, or under lib in a jre home") { root =>
    val jdk = jdk8(root.resolve("jdk"), "java/lang/Object.class" -> "jdk")
    val jre = root.resolve("jre")
    write(jre, "release", "JAVA_VERSION=\"1.8.0_402\"\n")
    jar(jre.resolve("lib/rt.jar"), "java/lang/Object.class" -> "jre")
    assertEquals(lookup(Some(jdk), Vector.empty, Vector.empty, "java/lang/Object"), Some("jdk"))
    assertEquals(lookup(Some(jre), Vector.empty, Vector.empty, "java/lang/Object"), Some("jre"))
  }

  withRoot("the target jdk is searched before the build outputs and the classpath") { root =>
    val jdk = jdk8(root.resolve("jdk"), "java/lang/Object.class" -> "jdk")
    write(root, "classes/java/lang/Object.class", "output")
    val library = root.resolve("lib.jar")
    jar(library, "java/lang/Object.class" -> "library")
    val outputs = Vector(root.resolve("classes"))
    assertEquals(lookup(Some(jdk), outputs, Vector(library), "java/lang/Object"), Some("jdk"))
    assertEquals(lookup(None, outputs, Vector(library), "java/lang/Object"), Some("output"))
  }

  withRoot("a classpath file that is not an archive holds no classes, and the entries after it are still read") {
    root =>
      write(root, "dependency.pom", "<project/>")
      val library = root.resolve("lib.jar")
      jar(library, "app/Main.class" -> "library")
      assertEquals(
        lookup(None, Vector.empty, Vector(root.resolve("dependency.pom"), library), "app/Main"),
        Some("library")
      )
  }

  withRoot("the build outputs are searched before the classpath") { root =>
    write(root, "classes/app/Main.class", "output")
    val library = root.resolve("lib.jar")
    jar(library, "app/Main.class" -> "library")
    assertEquals(lookup(None, Vector(root.resolve("classes")), Vector(library), "app/Main"), Some("output"))
  }

  withRoot("a classpath entry that does not exist is skipped") { root =>
    val library = root.resolve("lib.jar")
    jar(library, "app/Main.class" -> "library")
    assertEquals(
      lookup(None, Vector.empty, Vector(root.resolve("missing.jar"), library), "app/Main"),
      Some("library")
    )
  }

  withRoot("a directory on the classpath is read as a directory of classes") { root =>
    write(root, "lib/app/Main.class", "directory")
    assertEquals(lookup(None, Vector.empty, Vector(root.resolve("lib")), "app/Main"), Some("directory"))
  }

  withRoot("a class in no root is absent, and without a target jdk the jdk classes are too") { root =>
    val library = root.resolve("lib.jar")
    jar(library, "app/Main.class" -> "library")
    assertEquals(lookup(None, Vector.empty, Vector(library), "app/Missing"), None)
    assertEquals(lookup(None, Vector.empty, Vector.empty, "java/lang/Object"), None)
  }

  withRoot("a multi-release jar gives the class of the target release") { root =>
    val library = root.resolve("lib.jar")
    jar(library, multiRelease, "app/Main.class" -> "base", "META-INF/versions/9/app/Main.class" -> "nine")
    val jdk     = jdk8(root.resolve("jdk"), "java/lang/Object.class" -> "jdk")
    assertEquals(lookup(None, Vector.empty, Vector(library), "app/Main"), Some("base"))
    assertEquals(lookup(Some(jdk), Vector.empty, Vector(library), "app/Main"), Some("base"))
    assertEquals(lookup(Some(javaHome), Vector.empty, Vector(library), "app/Main"), Some("nine"))
  }

  withRoot("a jdk home without a release file, or without a class image, is an error naming the home") { root =>
    val bare = root.resolve("bare")
    assert(ClassSource.of(Vector.empty, Vector.empty, Some(bare)).left.exists(_.contains(bare.toString)))
    val eight = root.resolve("eight")
    write(eight, "release", "JAVA_VERSION=\"1.8.0_402\"\n")
    assert(ClassSource.of(Vector.empty, Vector.empty, Some(eight)).left.exists(_.contains(eight.toString)))
    val modular = root.resolve("modular")
    write(modular, "release", "JAVA_VERSION=\"25.0.1\"\n")
    assert(ClassSource.of(Vector.empty, Vector.empty, Some(modular)).left.exists(_.contains(modular.toString)))
  }

  withRoot("opening a source reads no class file") { root =>
    val jdk = root.resolve("jdk")
    write(jdk, "release", "JAVA_VERSION=\"1.8.0_402\"\n")
    write(jdk, "jre/lib/rt.jar", "not a zip")
    assert(ClassSource.of(Vector.empty, Vector.empty, Some(jdk)).isRight)
  }

  test("the classes of a jdk 9 or later are read from its jrt image") {
    val bytes = Using.resource(source(Vector.empty, Vector.empty, Some(javaHome)))(_.bytes("java/lang/Object")).get
    assertEquals(ByteBuffer.wrap(bytes).getInt(0), 0xcafebabe)
    assertEquals(ByteBuffer.wrap(bytes).getShort(6).toInt, sys.props("java.specification.version").toInt + 44)
  }
}
