package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.analysis.ClassFiles.*

class DictionariesSuite extends Graphed {
  private val main = Seq(
    "Dictionaries.scala" ->
      """package fixture
        |
        |trait Monad[F[_]] { def pure[A](a: A): F[A] }
        |
        |object Instances {
        |  given optionMonad: Monad[Option] with { def pure[A](a: A): Option[A] = Some(a) }
        |  given listMonad: Monad[List] with { def pure[A](a: A): List[A] = List(a) }
        |  given vectorMonad: Monad[Vector] with { def pure[A](a: A): Vector[A] = Vector(a) }
        |  given wrapped[F[_]](using m: Monad[F]): Monad[F] = m
        |}
        |
        |object Programs {
        |  def single[F[_]](using m: Monad[F]): F[Int] = m.pure(1)
        |  def several[F[_]](using m: Monad[F]): F[Int] = m.pure(2)
        |  def outer[F[_]](using m: Monad[F]): F[Int] = inner[F]
        |  def inner[F[_]](using m: Monad[F]): F[Int] = m.pure(3)
        |  def bound[F[_]: Monad]: F[Int] = single[F]
        |  def computed[F[_]](using m: Monad[F]): F[Int] = m.pure(4)
        |  def composite[F[_]](using m: Monad[F]): F[Int] = m.pure(5)
        |  def unused[F[_]](using m: Monad[F]): F[Int] = m.pure(6)
        |  def tested[F[_]](using m: Monad[F]): F[Int] = m.pure(7)
        |  def viaClass[F[_]](using m: Monad[F]): F[Int] = m.pure(8)
        |  def forwarded[F[_]](using m: Monad[F]): F[Int] = unused[F]
        |  def plain(x: Int): Int = x
        |}
        |
        |trait Runner { def run[F[_]](using m: Monad[F]): F[Int] }
        |object Doer extends Runner { def run[F[_]](using m: Monad[F]): F[Int] = m.pure(9) }
        |
        |class Service[F[_]](using m: Monad[F]) {
        |  def run: F[Int] = Programs.viaClass[F]
        |}
        |
        |object Main {
        |  import Instances.given
        |  def main(args: Array[String]): Unit = println((
        |    Programs.single[Option],
        |    Programs.several[Option],
        |    Programs.several[List],
        |    Programs.outer[Vector],
        |    Programs.bound[List],
        |    Programs.computed[Option](using fresh),
        |    Programs.composite[Option](using Instances.wrapped(using Instances.optionMonad)),
        |    Service[List]().run,
        |    (Doer: Runner).run[Option],
        |    Programs.plain(1)
        |  ))
        |  def fresh: Monad[Option] = new Monad[Option] { def pure[A](a: A): Option[A] = Some(a) }
        |}""".stripMargin
  )

  private val test = Seq(
    "Checks.scala" ->
      """package fixture
        |
        |class Checks {
        |  import Instances.given
        |  val tested = Programs.tested[Option]
        |  val forwarded = Programs.forwarded[Option]
        |}""".stripMargin
  )

  private lazy val parameters = inspect(main, test)((result, _) => result.dictionaries)

  private def of(method: String): Vector[Dictionaries.Parameter] =
    parameters.filter(_.method.startsWith(s"fixture.$method("))

  private def instances(method: String): Set[String] = of(method).flatMap(_.instances).toSet

  private def opened(method: String): Boolean = of(method).exists(_.open)

  test("a dictionary parameter takes the instance its only caller passes") {
    assertEquals(instances("Programs.single"), Set("fixture.Instances.optionMonad", "fixture.Instances.listMonad"))
    assert(!opened("Programs.single"))
  }

  test("a dictionary parameter takes every instance its callers pass") {
    assertEquals(instances("Programs.several"), Set("fixture.Instances.optionMonad", "fixture.Instances.listMonad"))
    assert(!opened("Programs.several"))
  }

  test("a given a caller forwards comes from the callers of that caller, through its context bound too") {
    assertEquals(instances("Programs.outer"), Set("fixture.Instances.vectorMonad"))
    assertEquals(instances("Programs.inner"), Set("fixture.Instances.vectorMonad"))
    assertEquals(instances("Programs.bound"), Set("fixture.Instances.listMonad"))
    assert(!opened("Programs.inner"))
  }

  test("a given a class takes reaches the methods that forward it") {
    assertEquals(instances("Service.<init>"), Set("fixture.Instances.listMonad"))
    assertEquals(instances("Programs.viaClass"), Set("fixture.Instances.listMonad"))
  }

  test("a value computed at run time is open, and an instance built from a given is named with its arguments") {
    assert(opened("Programs.computed"))
    assertEquals(
      instances("Programs.composite"),
      Set("fixture.Instances.wrapped(fixture.Instances.optionMonad)")
    )
    assert(!opened("Programs.composite"))
  }

  test("a method no production code calls is open, as is one forwarding a given of such a method") {
    assert(opened("Programs.unused"))
    assertEquals(instances("Programs.unused"), Set.empty[String])
    assert(opened("Programs.tested"))
    assert(opened("Programs.forwarded"))
  }

  test("an override takes what the callers of the method it overrides pass") {
    assertEquals(instances("Doer.run"), Set("fixture.Instances.optionMonad"))
    assertEquals(instances("Runner.run"), Set("fixture.Instances.optionMonad"))
  }

  test("a method without dictionary parameters has no entry") {
    assertEquals(of("Programs.plain"), Vector.empty)
  }

  test("the method of an entry is the key of its bytecode method") {
    graphed(main, test) { (result, _, world) =>
      val single = world.get("fixture/Programs$").toVector.flatMap(_.methods.filter(_.name == "single"))
      assertEquals(single.size, 1)
      val key = world.key(Reference("fixture/Programs$", "single", single.head.descriptor))
      assertEquals(result.dictionaries.filter(_.method == key).map(_.index), Vector(0))
    }
  }
}
