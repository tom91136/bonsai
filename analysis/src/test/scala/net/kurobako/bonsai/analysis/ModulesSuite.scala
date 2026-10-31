package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.java.{Model => JavaModel}
import net.kurobako.bonsai.testing.Scratch

class ModulesSuite extends munit.FunSuite {

  private def node(name: String, module: String, source: String): Node =
    Node(name, module, Output.Scope.Main, source, line = 1, generated = false, exported = false, outer = None)

  /** A build of an application module and a library module, with the requirement the application declares. */
  private def build(requires: String): (Manifest, Map[String, Node], Vector[JavaModel.Reflection]) = {
    val root       = Scratch.directory("bonsai-modules")
    val libClasses = Files.createDirectories(root.resolve("lib-classes"))
    val appClasses = Files.createDirectories(root.resolve("app-classes"))
    Files.createDirectories(libClasses.resolve("lib"))
    Files.writeString(libClasses.resolve("lib/Lib.class"), "")
    Files.createDirectories(appClasses.resolve("app"))
    Files.writeString(appClasses.resolve("app/App.class"), "")
    val library     = root.resolve("src/lib.module/module-info.java")
    val application = root.resolve("src/app.module/module-info.java")
    Files.createDirectories(library.getParent)
    Files.createDirectories(application.getParent)
    Files.writeString(library, "module lib.module { exports lib; }")
    Files.writeString(application, s"module app.module { $requires }")
    val manifest = Manifest(
      Vector(Output("lib", Output.Scope.Main, libClasses), Output("app", Output.Scope.Main, appClasses)),
      Vector.empty,
      Set.empty,
      sources = Vector(
        Output("lib", Output.Scope.Main, library),
        Output("app", Output.Scope.Main, application)
      )
    )
    val nodes = Map(
      "app.App" -> node("app.App", "app", "app/App.java"),
      "lib.Lib" -> node("lib.Lib", "lib", "lib/Lib.java")
    )
    (manifest, nodes, Vector(JavaModel.Reflection("app.App", "lib.Lib", "app/App.java:65")))
  }

  private def descriptors(text: String): Vector[Modules.Descriptor] =
    Scratch.using("module-") { directory =>
      val file = directory.resolve("module.java")
      Files.writeString(file, text)
      Modules.descriptor(file).toVector
    }

  test("a module that looks a class up without requiring its module is reported") {
    val (manifest, nodes, reflections) = build("")
    val uses                           = Modules.uses(manifest, nodes, reflections)

    assertEquals(uses.size, 1)
    assertEquals(uses.head.caller, "app.App")
    assertEquals(uses.head.source, "app/App.java")
    assertEquals(uses.head.line, 65)
    assertEquals(uses.head.callerModule, "app.module")
    assertEquals(uses.head.targetModule, "lib.module")
  }

  test("a lookup of a class of the caller's own module is not reported") {
    val (manifest, nodes, _) = build("")

    assertEquals(
      Modules.uses(manifest, nodes, Vector(JavaModel.Reflection("app.App", "app.App", "app/App.java:65"))),
      Vector.empty
    )
  }

  test("a caller whose module has no descriptor is not reported, since its classes read every module") {
    val (manifest, nodes, reflections) = build("")
    val undescribed                    = manifest.copy(sources = manifest.sources.filter(_.module == "lib"))

    assertEquals(Modules.uses(undescribed, nodes, reflections), Vector.empty)
  }

  test("a lookup of a class that no module of the build holds is not reported") {
    val (manifest, nodes, _) = build("")

    assertEquals(
      Modules.uses(manifest, nodes, Vector(JavaModel.Reflection("app.App", "sun.misc.Unsafe", "app/App.java:65"))),
      Vector.empty
    )
  }

  test("a module that requires the module of the class it looks up is not reported") {
    val (manifest, nodes, reflections) = build("requires lib.module;")

    assertEquals(Modules.uses(manifest, nodes, reflections), Vector.empty)
  }

  test("a module that reads another through a transitive requirement is not reported") {
    Scratch.using("bonsai-modules") { root =>
      val libClasses    = Files.createDirectories(root.resolve("lib-classes"))
      val bridgeClasses = Files.createDirectories(root.resolve("bridge-classes"))
      val appClasses    = Files.createDirectories(root.resolve("app-classes"))
      Files.createDirectories(libClasses.resolve("lib"))
      Files.writeString(libClasses.resolve("lib/Lib.class"), "")
      Files.createDirectories(bridgeClasses.resolve("bridge"))
      Files.writeString(bridgeClasses.resolve("bridge/Bridge.class"), "")
      Files.createDirectories(appClasses.resolve("app"))
      Files.writeString(appClasses.resolve("app/App.class"), "")
      def source(module: String, text: String): Path = {
        val file = root.resolve(s"src/$module/module-info.java")
        Files.createDirectories(file.getParent)
        Files.writeString(file, text)
        file
      }
      val manifest = Manifest(
        Vector(
          Output("lib", Output.Scope.Main, libClasses),
          Output("bridge", Output.Scope.Main, bridgeClasses),
          Output("app", Output.Scope.Main, appClasses)
        ),
        Vector.empty,
        Set.empty,
        sources = Vector(
          Output("lib", Output.Scope.Main, source("lib.module", "module lib.module { exports lib; }")),
          Output(
            "bridge",
            Output.Scope.Main,
            source("bridge.module", "module bridge.module { requires transitive lib.module; }")
          ),
          Output(
            "app",
            Output.Scope.Main,
            source("app.module", "module app.module { requires bridge.module; }")
          )
        )
      )
      val nodes = Map(
        "app.App" -> node("app.App", "app", "app/App.java"),
        "lib.Lib" -> node("lib.Lib", "lib", "lib/Lib.java")
      )

      assertEquals(
        Modules.uses(manifest, nodes, Vector(JavaModel.Reflection("app.App", "lib.Lib", "app/App.java:65"))),
        Vector.empty
      )
    }
  }

  test("a module descriptor names its requirements, including the ones that are transitive") {
    val found = descriptors("""
      |/** A module. */
      |module demo.module {
      |  requires transitive lib.module;
      |  requires static optional.module;
      |  requires java.base;
      |}
      |""".stripMargin)

    assertEquals(found.map(_.name), Vector("demo.module"))
    assertEquals(
      found.head.requires,
      Vector("lib.module" -> true, "optional.module" -> false, "java.base" -> false)
    )
  }
}
