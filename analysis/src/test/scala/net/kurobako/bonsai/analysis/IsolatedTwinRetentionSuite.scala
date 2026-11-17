package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.testing.*
import scala.collection.mutable

/** Runs in a JVM of its own, so the compiler contexts it counts are the ones it creates. */
class IsolatedTwinRetentionSuite extends AnalysisSuite {

  private class Watched(outputs: Set[Output], watch: () => Unit) extends Set[Output] {
    def contains(output: Output): Boolean = { watch(); outputs.contains(output) }
    def incl(output: Output): Set[Output] = Watched(outputs + output, watch)
    def excl(output: Output): Set[Output] = Watched(outputs - output, watch)
    // scalafix:off BannedSyntax.no-iterator
    def iterator: Iterator[Output] = outputs.iterator
    // scalafix:on BannedSyntax.no-iterator
  }

  test("a platform twin's compiler state is released once it is read") {
    Scratch.using("bonsai-") { directory =>
      val shared = Seq(
        "A.scala" -> "package lib\nobject A { def one: Int = 1 }",
        "B.scala" -> "package lib\nclass B { def two: Int = A.one }"
      )
      val core  = compile(directory.resolve("core"), shared, Vector.empty)
      val other =
        compile(directory.resolve("other"), Seq("C.scala" -> "object C { def three: Int = lib.A.one }"), Vector(core))
      val twins = Vector("t0", "t1").map { module =>
        val own = Seq(s"Own$module.scala" -> s"object Own$module { def go: Int = lib.A.one }")
        Output(module, Output.Scope.Main, compile(directory.resolve(module), shared ++ own, Vector.empty))
      }
      val outputs  = twins :+ Output("core", Output.Scope.Main, core)
      val group    = Output("other", Output.Scope.Main, other)
      val manifest = Manifest(
        outputs :+ group,
        scoped((outputs :+ group).map(_.module).map(_ -> classpath).toMap),
        Set.empty,
        sources = Vector(
          Output("core", Output.Scope.Main, directory.resolve("core/A.scala")),
          Output("core", Output.Scope.Main, directory.resolve("core/B.scala")),
          Output("other", Output.Scope.Main, directory.resolve("other/C.scala"))
        ) ++ twins.flatMap { twin =>
          Vector("A.scala", "B.scala", s"Own${twin.module}.scala").map(name =>
            Output(twin.module, Output.Scope.Main, directory.resolve(twin.module).resolve(name))
          )
        },
        platforms = twins.map(_.module).toSet,
        jdk = targetJdk
      )
      val baseline = Heap.live("dotty.tools.dotc.core.Contexts$ContextBase")
      val retained = mutable.ArrayBuffer.empty[Int]
      val watch = () => if (retained.isEmpty) retained += Heap.live("dotty.tools.dotc.core.Contexts$ContextBase"): Unit
      val settings =
        Extraction.Settings(Vector.empty, Set.empty, Vector.empty, Vector.empty, Vector.empty, Parallel.sequential)
      val result = Extraction.readDivided(
        manifest,
        settings,
        (_, _, _) => Right(Clashes.Divided(Vector(outputs.toSet, Watched(Set(group), watch)), Vector.empty))
      ).fold(errors => fail(errors.mkString("\n")), identity)
      assertEquals(result.problems, Vector.empty)
      assertEquals(retained.toVector, Vector(baseline))
    }
  }
}
