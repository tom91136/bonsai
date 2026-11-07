package net.kurobako.bonsai.plugin

import dotty.tools.dotc.interactive.InteractiveDriver
import java.net.URI
import munit.FunSuite

class InteractiveSuite extends FunSuite {

  // Compiler lines differ in the logical root package the constructor takes after the settings, if any.
  private def driver(settings: List[String]): InteractiveDriver = {
    val constructor = classOf[InteractiveDriver].getConstructors.head
    val roots       = constructor.getParameterTypes.toList.drop(1).map[AnyRef] { root =>
      if (root == classOf[Option[?]]) None else root.getMethod("none").invoke(null) // scalafix:ok DisableSyntax.null
    }
    constructor.newInstance((settings :: roots)*).asInstanceOf[InteractiveDriver]
  }

  test("an interactive compiler types sources with the plugin loaded and runs none of its checks") {
    val compiler = driver(
      List(
        "-classpath",
        sys.props("bonsai.plugin.classpath"),
        s"-Xplugin:${sys.props("bonsai.plugin.jar")}",
        "-Xplugin-require:bonsai",
        "-color:never",
        "-P:bonsai:mode=unknown"
      )
    )
    val source = URI("file:///A.scala")
    assertEquals(compiler.run(source, "object A { val names = List(1) }").map(_.message), Nil)
    assert(compiler.openedTrees(source).nonEmpty)
  }
}
