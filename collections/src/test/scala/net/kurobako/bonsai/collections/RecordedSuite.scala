package net.kurobako.bonsai.collections

import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.Path
import munit.FunSuite
import scala.jdk.CollectionConverters.*

class RecordedSuite extends FunSuite {
  private val output = FunFixture[Path](_ => Files.createTempDirectory("bonsai-recorded-"), dir => deleteAll(dir))

  private def deleteAll(path: Path): Unit = {
    if (Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)) Files.list(path).toList.asScala.foreach(deleteAll(_))
    Files.delete(path)
  }

  private val record = Recorded.Record(
    "src/main/scala/demo/Main.scala",
    Recorded.digest("object Main".getBytes("UTF-8")),
    List(
      Recorded.Entry("identical-branches", Recorded.Level.Error, 4, "both branches\tare the same\nsecond line\\end"),
      Recorded.Entry("long-arguments", Recorded.Level.Warning, 9, "plain"),
      Recorded.Entry("inferred-refined-type", Recorded.Level.Info, 12, "")
    )
  )

  test("a record reads back as written, whatever its messages hold") {
    assertEquals(Recorded.parse(Recorded.render(record)), Some(record))
  }

  test("a record with no findings is a record all the same") {
    val empty = record.copy(entries = Nil)
    assertEquals(Recorded.parse(Recorded.render(empty)), Some(empty))
  }

  test("text that is not a record does not parse") {
    assertEquals(Recorded.parse(""), None)
    assertEquals(Recorded.parse("source\tonly"), None)
    assertEquals(Recorded.parse(Recorded.render(record).replace("identical-branches\terror\t4", "x\tfatal\t4")), None)
    assertEquals(Recorded.parse(Recorded.render(record).replace("\t4\t", "\tfour\t")), None)
  }

  test("the same source has the same file, and two sources have two") {
    val directory = Path.of("classes")
    assertEquals(Recorded.file(directory, "a/B.scala"), Recorded.file(directory, "a/B.scala"))
    assertNotEquals(Recorded.file(directory, "a/B.scala"), Recorded.file(directory, "a/C.scala"))
    assert(Recorded.file(directory, "a/B.scala").startsWith(directory.resolve(Recorded.Directory)))
  }

  test("a source is named from the root with slashes, or by its path when outside it") {
    val root = Path.of("/work/build")
    assertEquals(Recorded.key(root, Path.of("/work/build/app/src/A.scala")), "app/src/A.scala")
    assertEquals(Recorded.key(root, Path.of("/work/build/app/../app/src/A.scala")), "app/src/A.scala")
    assertEquals(Recorded.key(root, Path.of("/elsewhere/A.scala")), "/elsewhere/A.scala")
  }

  output.test("a written record is the one read, and a later write replaces it") { directory =>
    Recorded.write(directory, record)
    assertEquals(Recorded.read(Recorded.file(directory, record.source)), Some(record))
    val later = record.copy(digest = Recorded.digest("object Other".getBytes("UTF-8")), entries = Nil)
    Recorded.write(directory, later)
    assertEquals(Recorded.read(Recorded.file(directory, record.source)), Some(later))
    assertEquals(Recorded.all(directory).map(_._2), Vector(Some(later)))
  }

  output.test("a record that is a link into a store is replaced, and the store is left as it was") { directory =>
    val store  = Files.createTempDirectory("bonsai-store-")
    val stored = store.resolve("record.tsv")
    Files.writeString(stored, Recorded.render(record))
    val link = Recorded.file(directory, record.source)
    Files.createDirectories(link.getParent)
    Files.createSymbolicLink(link, stored)
    val later = record.copy(entries = Nil)
    Recorded.write(directory, later)
    assertEquals(Recorded.read(Recorded.file(directory, record.source)), Some(later))
    assertEquals(Recorded.parse(Files.readString(stored)), Some(record))
    assert(!Files.isSymbolicLink(link))
    deleteAll(store)
  }

  output.test("a directory with no records lists none") { directory =>
    assertEquals(Recorded.all(directory), Vector.empty)
    assertEquals(Recorded.read(Recorded.file(directory, "A.scala")), None)
  }
}
