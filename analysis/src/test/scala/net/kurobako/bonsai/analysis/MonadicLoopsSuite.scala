package net.kurobako.bonsai.analysis

class MonadicLoopsSuite extends AnalysisSuite {
  private val effects =
    """package fixture
      |
      |final class Eff[A](val run: () => A) {
      |  def flatMap[B](f: A => Eff[B]): Eff[B] = Eff(() => f(run()).run())
      |  def >>[B](next: => Eff[B]): Eff[B] = flatMap(_ => next)
      |  def foreverM: Eff[Nothing] = Eff(() => { while (true) run(); throw IllegalStateException() })
      |}
      |
      |object Eff { def apply[A](run: () => A): Eff[A] = new Eff(run) }
      |
      |trait Monad[F[_]] {
      |  def pure[A](a: A): F[A]
      |  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
      |  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
      |}
      |
      |object Monad { def apply[F[_]](using m: Monad[F]): Monad[F] = m }
      |
      |extension [F[_], A](fa: F[A])(using m: Monad[F]) {
      |  def flatMap[B](f: A => F[B]): F[B] = m.flatMap(fa)(f)
      |  def >>[B](next: => F[B]): F[B] = m.flatMap(fa)(_ => next)
      |  def foreverM: F[Nothing] = m.flatMap(fa)(_ => fa.foreverM)
      |}
      |
      |trait Shape { def area: Int }
      |class Circle extends Shape { def area = 1 }
      |class Square extends Shape { def area = 2 }
      |class Triangle extends Shape { def area = 3 }""".stripMargin

  private val programs =
    """package fixture
      |
      |object Programs {
      |  def step(a: Int): Eff[Int] = Eff(() => a)
      |  def flatMapped(s: Shape): Eff[Unit] = { val a = s.area; step(a).flatMap(_ => flatMapped(s)) }
      |  def sequenced(s: Shape): Eff[Unit] = { val a = s.area; step(a) >> sequenced(s) }
      |  def forever(s: Shape): Eff[Nothing] = { val a = s.area; step(a).foreverM }
      |  def counted(s: Shape, n: Int): Eff[Int] = {
      |    val a = s.area
      |    step(a).flatMap(i => if (n > 0) counted(s, n - 1) else Eff(() => i))
      |  }
      |  def tagless[F[_]: Monad](s: Shape, n: Int): F[Unit] = {
      |    val a = s.area
      |    Monad[F].pure(n + a).flatMap(_ => tagless[F](s, n))
      |  }
      |  def taglessSequenced[F[_]: Monad](s: Shape): F[Unit] = {
      |    val a = s.area
      |    Monad[F].pure(a) >> taglessSequenced[F](s)
      |  }
      |  def taglessForever[F[_]: Monad](s: Shape): F[Nothing] = { val a = s.area; Monad[F].pure(a).foreverM }
      |  def rec[F[_]](s: Shape, n: Int)(using m: Monad[F]): F[Int] = {
      |    val a = s.area
      |    Monad[F].tailRecM(n)(i => m.pure(if (i > a) Left(i - 1) else Right(i)))
      |  }
      |  def effectRec(s: Shape, n: Int)(using m: Monad[Eff]): Eff[Int] = {
      |    val a = s.area
      |    Monad[Eff].tailRecM(n)(i => Eff(() => if (i > a) Left(i - 1) else Right(i)))
      |  }
      |  def listed(s: Shape, xs: List[Int]): List[Int] = { val a = s.area; xs.flatMap(x => listed(s, List(x + a))) }
      |  def optional(s: Shape, n: Int): Option[Int] = {
      |    val a = s.area
      |    Some(n).flatMap(m => if (m > a) optional(s, m - 1) else None)
      |  }
      |  def other(s: Shape): Eff[Int] = { val a = s.area; step(a) }
      |  def delegating(s: Shape): Eff[Int] = { val a = s.area; step(a).flatMap(_ => other(s)) }
      |  def returning(s: Shape): Eff[Int] = { val a = s.area; step(a) }
      |  def unrelated(s: Shape): Eff[Int] = { val a = s.area; step(a).flatMap(_ => delegating(s)) }
      |}""".stripMargin

  private val main =
    """package fixture
      |
      |object Main {
      |  given Monad[Eff] with {
      |    def pure[A](a: A): Eff[A] = Eff(() => a)
      |    def flatMap[A, B](fa: Eff[A])(f: A => Eff[B]): Eff[B] = fa.flatMap(f)
      |    def tailRecM[A, B](a: A)(f: A => Eff[Either[A, B]]): Eff[B] = f(a).flatMap {
      |      case Left(next)  => tailRecM(next)(f)
      |      case Right(done) => pure(done)
      |    }
      |  }
      |  def main(args: Array[String]): Unit = {
      |    val all = List[Shape](Circle(), Square(), Triangle())
      |    all.foreach { s =>
      |      println((
      |        Programs.flatMapped(s), Programs.sequenced(s), Programs.forever(s), Programs.counted(s, 1),
      |        Programs.tagless[Eff](s, 1), Programs.taglessSequenced[Eff](s), Programs.taglessForever[Eff](s),
      |        Programs.rec[Eff](s, 1), Programs.effectRec(s, 1), Programs.listed(s, Nil), Programs.optional(s, 1),
      |        Programs.other(s), Programs.delegating(s), Programs.returning(s), Programs.unrelated(s)
      |      ))
      |    }
      |  }
      |}""".stripMargin

  private lazy val looped: Set[String] = {
    val result = extract(
      Seq("Main.scala" -> main, "Effects.scala" -> effects, "Programs.scala" -> programs),
      hot = Vector("**")
    )
    Megamorphic.findings(result.megamorphic, result.graph.nodes).map(_.detail.takeWhile(_ != ',')).toSet
  }

  private def loops(name: String): Boolean = looped.contains(s"Programs.$name calls Shape.area in a loop")

  test("a method returning an effect that flatMaps into itself is a loop") {
    assert(loops("flatMapped"))
    assert(loops("counted"))
  }

  test("a method returning an effect that continues into itself by the by-name argument of >> is a loop") {
    assert(loops("sequenced"))
  }

  test("a method returning an effect it repeats with foreverM is a loop") {
    assert(loops("forever"))
  }

  test("a method returning an effect that steps with tailRecM is a loop") {
    assert(loops("rec"))
    assert(loops("effectRec"))
  }

  test("the same holds for a tagless effect that only a Monad instance defines") {
    assert(loops("tagless"))
    assert(loops("taglessSequenced"))
    assert(loops("taglessForever"))
  }

  test("flatMap of a collection or an option that recurses runs at once and is no loop") {
    assert(!loops("listed"))
    assert(!loops("optional"))
  }

  test("an effect that calls another method, or only returns an effect, is no loop") {
    assert(!loops("delegating"))
    assert(!loops("other"))
    assert(!loops("returning"))
    assert(!loops("unrelated"))
  }
}
