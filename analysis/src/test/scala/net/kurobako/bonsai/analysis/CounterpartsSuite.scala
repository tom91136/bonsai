package net.kurobako.bonsai.analysis

class CounterpartsSuite extends AnalysisSuite {
  private def findings(
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil
  ): Vector[Finding] = {
    val result = extract(main, test)
    Structures.findings(result.structures, result.graph)
  }

  private def of(found: Vector[Finding], rule: Finding.Rule): Vector[Finding] = found.filter(_.rule == rule)

  private val Planning =
    """package plan
      |enum Kind { case Convert, Recode }
      |case class Job(id: Int, name: String, kind: Kind)
      |case class Plan(jobs: Vector[Job])
      |case class Estimate(bytes: Long)
      |object Planner {
      |  def existingTarget(name: String): Option[String] = Option.when(name.isEmpty)(name)
      |  def estimate(name: String): Estimate = Estimate(name.length.toLong)
      |  def stamp(name: String): String = name.trim
      |  def lookup(name: String): Int = name.length
      |  def planRoot(root: String): Plan =
      |    Plan(Vector(root).flatMap(name =>
      |      existingTarget(name) match {
      |        case Some(_) => None
      |        case None    => Some(Job(estimate(stamp(name)).bytes.toInt + Estimate(0L).bytes.toInt, stamp(name), Kind.Convert))
      |      }
      |    ))
      |}
      |object Vncg {
      |  def plan(source: String, extra: Int): Plan =
      |    Plan(Vector(source).flatMap(entry =>
      |      Planner.existingTarget(entry).fold(Some(Job(Planner.estimate(Planner.stamp(entry)).bytes.toInt + Estimate(1L).bytes.toInt + extra, Planner.stamp(entry), Kind.Convert)))(_ => None)
      |    ))
      |  def unrelated(name: String): Int = Planner.lookup(name) + Planner.lookup(Planner.stamp(name))
      |}""".stripMargin

  test("two methods that use the same rarely used symbols of the build are parallel implementations") {
    val found = of(findings(Seq("Planning.scala" -> Planning)), Finding.Rule.ParallelImplementation)
    assertEquals(found.size, 1, found.mkString("\n"))
    assertEquals(found.head.level, Finding.Level.Info)
    assert(found.head.detail.contains("Planner.planRoot and Vncg.plan"), found.head.detail)
    assert(found.head.detail.contains("Estimate, Job, Kind, Plan"), found.head.detail)
  }

  private val Mirroring =
    """package conv
      |enum Outcome[+R] {
      |  case Done(value: R)
      |  case Failed(error: Throwable)
      |  case Rejected(reason: String)
      |}
      |enum Result {
      |  case Converted(bytes: Int)
      |  case Failed(message: String)
      |  case Rejected(reason: String)
      |}
      |enum Verdict {
      |  case Kept
      |  case Lost(reason: String)
      |}
      |object Runner {
      |  def toResult(outcome: Outcome[Result]): Result = outcome match {
      |    case Outcome.Done(result)       => result
      |    case Outcome.Failed(error)      => Result.Failed(error.getMessage)
      |    case Outcome.Rejected(because) => Result.Rejected(because)
      |  }
      |  def toVerdict(outcome: Outcome[Verdict]): Verdict = outcome match {
      |    case Outcome.Done(verdict)  => verdict
      |    case Outcome.Failed(error)  => Verdict.Lost(error.getMessage)
      |    case Outcome.Rejected(why)  => Verdict.Lost(why)
      |  }
      |  def renamed(result: Result): Verdict = result match {
      |    case Result.Converted(_) => Verdict.Kept
      |    case Result.Failed(m)    => Verdict.Lost(m)
      |    case Result.Rejected(m)  => Verdict.Lost(m)
      |  }
      |}""".stripMargin

  test("a match that turns cases into same-named cases of another enum mirrors it") {
    val found = of(findings(Seq("Mirroring.scala" -> Mirroring)), Finding.Rule.MirroredCases)
    assertEquals(found.size, 1, found.mkString("\n"))
    assertEquals(found.head.level, Finding.Level.Info)
    assert(
      found.head.detail.startsWith("Runner.toResult turns Failed, Rejected of Outcome into the cases of Result"),
      found.head.detail
    )
  }

  private val Families =
    """package fam
      |object Pipeline {
      |  case class Job(id: Int)
      |  case class Plan(jobs: Vector[Job])
      |  case class Report(plan: Plan)
      |  enum Result { case Kept; case Lost }
      |}
      |object Video {
      |  case class Job(path: String)
      |  case class Plan(jobs: Vector[Job])
      |  case class Report(plan: Plan)
      |  enum Result { case Kept; case Replaced }
      |}
      |object Misc {
      |  case class Job(name: String)
      |  case class Plan(steps: Int)
      |  case class Other(count: Int)
      |}""".stripMargin

  test("objects that define the same nested types are a parallel family") {
    val found = of(findings(Seq("Families.scala" -> Families)), Finding.Rule.ParallelFamily)
    assertEquals(
      found.map(_.detail.takeWhile(_ != ';')),
      Vector("Pipeline and Video both define Job, Plan, Report, Result")
    )
    assertEquals(found.head.level, Finding.Level.Info)
  }

  private val Determined =
    """package det
      |enum Kind { case Convert, Recode, Sweep }
      |enum Mode { case Fast, Slow }
      |case class Job(id: Int, kind: Kind, nested: Boolean, depth: Int)
      |case class Config(mode: Mode, verbose: Boolean)
      |object Planner {
      |  def planRoot(names: Vector[String]): Vector[Job] = {
      |    var id   = 0
      |    val jobs = Vector.newBuilder[Job]
      |    def job(kind: Kind, nested: Boolean, depth: Int): Unit = {
      |      jobs += Job(id, kind, nested, depth)
      |      id += 1
      |    }
      |    names.foreach { name =>
      |      if (name.endsWith(".zip")) job(Kind.Recode, false, name.length)
      |      else if (name.endsWith(".rar")) job(Kind.Recode, false, 2)
      |      else if (name.endsWith(".tar")) job(Kind.Recode, false, 3)
      |      else if (name.endsWith(".7z")) job(Kind.Convert, name.length > 3, 1)
      |      else if (name.isEmpty) job(Kind.Convert, false, 1)
      |      else job(Kind.Sweep, name.length > 4, 0)
      |    }
      |    jobs.result()
      |  }
      |  def configs(flag: Boolean): Vector[Config] =
      |    Vector(Config(Mode.Fast, true), Config(Mode.Fast, false), Config(Mode.Slow, flag), Config(Mode.Slow, true))
      |}""".stripMargin

  test("a field a case of another field fixes is determined by it, also through a local helper") {
    val result = extract(Seq("Determined.scala" -> Determined))
    val found  = of(Algebra.findings(result.algebra, result.graph.nodes), Finding.Rule.CaseDeterminedField)
    assertEquals(
      found.map(_.detail.takeWhile(_ != ';')),
      Vector("Job.nested is always False when kind is Recode across 3 constructions")
    )
    assertEquals(found.head.level, Finding.Level.Info)
  }
}
