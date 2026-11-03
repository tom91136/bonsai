package net.kurobako.bonsai.analysis

import java.nio.file.Files
import net.kurobako.bonsai.testing.Scratch

class OutputsSuite extends munit.FunSuite {

  test("the files of an output are listed in path order whatever order they were written in") {
    val names = Seq("a/One.tasty", "a/Two.tasty", "b/Three.tasty", "b/c/Four.tasty", "Five.tasty", "z/Six.tasty")
    val roots = Seq(names, names.reverse, names.drop(3) ++ names.take(3)).map { order =>
      val root = Scratch.directory("bonsai-outputs")
      order.foreach { name =>
        val file = root.resolve(name)
        Files.createDirectories(file.getParent)
        Files.writeString(file, "")
      }
      root
    }
    val listings = roots.map(root => Outputs.files(root).map(root.relativize(_).toString))
    assertEquals(listings.map(_.toSet).distinct.size, 1)
    listings.foreach(listing => assertEquals(listing, listing.sorted))
  }
}
