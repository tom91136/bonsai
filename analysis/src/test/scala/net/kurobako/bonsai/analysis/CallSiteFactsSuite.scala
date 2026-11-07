package net.kurobako.bonsai.analysis

import java.nio.file.Path
import net.kurobako.bonsai.analysis.CallGraph.*
import net.kurobako.bonsai.analysis.ClassFiles.*
import net.kurobako.bonsai.testing.*
import scala.util.Using

class CallSiteFactsSuite extends AnalysisSuite {
  private val sources = Seq(
    "Fixture.scala" ->
      """package fixture
        |
        |@FunctionalInterface
        |trait Step { def run(): Int }
        |class First extends Step { def run(): Int = 1 }
        |class Second extends Step { def run(): Int = 2 }
        |class Never extends Step { def run(): Int = 3 }
        |trait Other { def run(): Int }
        |class Unrelated { def run(): Int = 5 }
        |
        |object Main {
        |  def main(args: Array[String]): Unit = {
        |    val lambda: Step = () => 4
        |    val other: Other = () => 9
        |    println((first(First()), second(Second()), third(lambda), sizes(), other.run(), Unrelated().run()))
        |  }
        |  def first(step: Step): Int = shared(step)
        |  def second(step: Step): Int = shared(step) + step.run()
        |  def third(step: Step): Int = lone(step)
        |  def shared(step: Step): Int = step.run()
        |  def lone(step: Step): Int = step.run()
        |  def sizes(): Int = {
        |    val array = java.util.ArrayList[Int]()
        |    val linked = java.util.LinkedList[Int]()
        |    count(array) + count(linked)
        |  }
        |  def count(values: java.util.List[Int]): Int = values.size
        |}""".stripMargin
  )

  private def compiled[A](roots: Vector[Reference])(use: (CallGraph, ClassWorld) => A): A =
    Scratch.using("bonsai-graph-") { directory =>
      ClassSource.of(
        Vector(compile(directory, sources, Vector.empty)),
        classpath,
        Some(Path.of(sys.props("java.home")))
      ) match {
        case Left(error)   => fail(error)
        case Right(source) =>
          Using.resource(source) { source =>
            val world = ClassWorld(source.bytes(_))
            use(CallGraph.build(world, roots.toSet), world)
          }
      }
    }

  private val step  = Reference("fixture/Step", "run", "()I")
  private val entry = Reference("fixture/Main", "main", "([Ljava/lang/String;)V")

  test("the receivers of a callee are the classes and lambdas that reach it, and nothing never instantiated") {
    compiled(Vector(entry)) { (graph, world) =>
      val receivers = graph.receivers(step, world)
      assertEquals(
        receivers.collect { case Receiver.Instance(name) => name }.toSet,
        Set("fixture/First", "fixture/Second")
      )
      assertEquals(receivers.collect { case Receiver.Closure(site) => site.holder.owner }, Vector("fixture/Main$"))
    }
  }

  test("a library class is a receiver as a build class is") {
    compiled(Vector(entry)) { (graph, world) =>
      val names = graph.receivers(Reference("java/util/List", "size", "()I"), world).collect {
        case Receiver.Instance(name) => name
      }
      assert(names.contains("java/util/ArrayList") && names.contains("java/util/LinkedList"), names.toString)
    }
  }
}
