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
        |    println((first(First()), second(Second()), third(lambda), sizes(), other.run(), Unrelated().run(), Flow.all()))
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
        |}
        |
        |class Runner { def go(step: Step): Int = step.run() }
        |class Holder(val step: Step) { def held(): Int = step.run() }
        |
        |object Flow {
        |  def all(): Int =
        |    made() + looped(First(), 2) + Runner().go(Second()) + Holder(First()).held() + mapped(_.run(), Second()) +
        |      mixed(true, First()) + captured(First())
        |  def made(): Int = { val step: Step = Second(); step.run() }
        |  def looped(step: Step, n: Int): Int = if (n == 0) step.run() else looped(step, n - 1)
        |  def mapped(f: Step => Int, step: Step): Int = f(step)
        |  def captured(step: Step): Int = mapped(_ => step.run(), Second())
        |  def mixed(flag: Boolean, step: Step): Int = (if (flag) step else Second()).run()
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

  private def ref(owner: String, name: String, descriptor: String = "()I"): Reference =
    Reference(s"fixture/$owner", name, descriptor)

  private val stepped = "(Lfixture/Step;)I"
  private val step    = Reference("fixture/Step", "run", "()I")
  private val entry   = Reference("fixture/Main", "main", "([Ljava/lang/String;)V")

  private def instances(supply: ReceiverFlow.Supply): Set[String] =
    supply.receivers.collect { case Receiver.Instance(name) => name }

  private def flowed[A](roots: Vector[Reference])(use: (ReceiverFlow, ClassWorld) => A): A =
    compiled(roots)((graph, world) => use(ReceiverFlow(world, graph), world))

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

  test("the callers of a method are the methods reaching it by a direct call or by dispatch") {
    flowed(Vector(entry)) { (flow, _) =>
      assertEquals(
        flow.callers(ref("Main$", "shared", stepped)),
        Set(ref("Main$", "first", stepped), ref("Main$", "second", stepped))
      )
      assertEquals(flow.callers(ref("Main$", "lone", stepped)), Set(ref("Main$", "third", stepped)))
      assertEquals(flow.callers(ref("Runner", "go", stepped)), Set(ref("Flow$", "all")))
    }
  }

  test("a shared body's receivers are told apart by the caller that passes them") {
    flowed(Vector(entry)) { (flow, _) =>
      val supplied = flow.supplied(ref("Main$", "shared", stepped), step)
      assertEquals(supplied.byCaller.keySet, Set(ref("Main$", "first", stepped), ref("Main$", "second", stepped)))
      assertEquals(instances(supplied.byCaller(ref("Main$", "first", stepped))), Set("fixture/First"))
      assertEquals(instances(supplied.byCaller(ref("Main$", "second", stepped))), Set("fixture/Second"))
      assert(supplied.byCaller.values.forall(!_.open))
      assertEquals(supplied.own, ReceiverFlow.Supply(Set.empty, false))
    }
  }

  test("a method that holds a call and forwards its receiver is supplied by its own caller") {
    flowed(Vector(entry)) { (flow, _) =>
      val supplied = flow.supplied(ref("Main$", "second", stepped), step)
      assertEquals(supplied.byCaller.keySet, Set(ref("Main$", "main", "([Ljava/lang/String;)V")))
      assertEquals(instances(supplied.byCaller.values.head), Set("fixture/Second"))
    }
  }

  test("a lambda passed to a body is the receiver of its caller") {
    flowed(Vector(entry)) { (flow, _) =>
      val supplied = flow.supplied(ref("Main$", "lone", stepped), step)
      val third    = supplied.byCaller(ref("Main$", "third", stepped))
      assertEquals(instances(third), Set.empty[String])
      assertEquals(third.receivers.collect { case Receiver.Closure(site) => site.holder.name }, Set("main"))
      assert(!third.open)
    }
  }

  test("a receiver a method creates itself is its own") {
    flowed(Vector(entry)) { (flow, _) =>
      val supplied = flow.supplied(ref("Flow$", "made"), step)
      assertEquals(instances(supplied.own), Set("fixture/Second"))
      assert(!supplied.own.open)
      assertEquals(supplied.byCaller, Map.empty[Reference, ReceiverFlow.Supply])
    }
  }

  test("a receiver read from a field is open") {
    flowed(Vector(entry)) { (flow, _) =>
      val supplied = flow.supplied(ref("Holder", "held"), step)
      assert(supplied.own.open)
      assertEquals(supplied.own.receivers, Set.empty[Receiver])
    }
  }

  test("a receiver that is a parameter or created here is both") {
    flowed(Vector(entry)) { (flow, _) =>
      val supplied = flow.supplied(ref("Flow$", "mixed", "(ZLfixture/Step;)I"), step)
      assertEquals(instances(supplied.own), Set("fixture/Second"))
      val caller = supplied.byCaller(ref("Flow$", "all"))
      assertEquals(instances(caller), Set("fixture/First", "fixture/Second"))
    }
  }

  test("a loop through a recursive call settles on the receivers its entry passes") {
    flowed(Vector(entry)) { (flow, _) =>
      val supplied = flow.supplied(ref("Flow$", "looped", "(Lfixture/Step;I)I"), step)
      assertEquals(
        supplied.byCaller.view.mapValues(instances(_)).toMap,
        Map(ref("Flow$", "all") -> Set("fixture/First"))
      )
    }
  }

  test("a method reached by dispatch takes the receiver of the call as well as its arguments") {
    flowed(Vector(entry)) { (flow, _) =>
      val go = flow.supplied(ref("Runner", "go", stepped), step)
      assertEquals(go.byCaller.view.mapValues(instances(_)).toMap, Map(ref("Flow$", "all") -> Set("fixture/Second")))
    }
  }

  test("the argument of a lambda comes from the call of the function") {
    flowed(Vector(entry)) { (flow, _) =>
      val body = flow.callers(ref("Flow$", "mapped", "(Lscala/Function1;Lfixture/Step;)I"))
      assertEquals(body, Set(ref("Flow$", "all"), ref("Flow$", "captured", stepped)))
      val lambda = ref("Flow$", "all$$anonfun$1", stepped)
      assert(flow.holders(step)(lambda))
      val supplied = flow.supplied(lambda, step)
      assertEquals(supplied.byCaller.values.flatMap(instances(_)).toSet, Set("fixture/Second"))
    }
  }

  test("a value a lambda captures is open, since the closure is made elsewhere") {
    flowed(Vector(entry)) { (flow, _) =>
      val supplied = flow.supplied(ref("Flow$", "captured$$anonfun$1", "(Lfixture/Step;Lfixture/Step;)I"), step)
      assertEquals(supplied.byCaller.keySet, Set(ref("Flow$", "mapped", "(Lscala/Function1;Lfixture/Step;)I")))
      assert(supplied.byCaller.values.forall(_.open))
    }
  }

  test("a method nothing calls is entered from outside, so its parameter is open") {
    flowed(Vector(ref("Main$", "lone", stepped))) { (flow, _) =>
      val supplied = flow.supplied(ref("Main$", "lone", stepped), step)
      assert(supplied.own.open)
      assertEquals(supplied.byCaller, Map.empty[Reference, ReceiverFlow.Supply])
    }
  }
}
