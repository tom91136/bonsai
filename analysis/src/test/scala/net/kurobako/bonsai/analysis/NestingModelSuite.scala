package net.kurobako.bonsai.analysis

import munit.FunSuite
import scala.util.Random

/** The target placement over graphs written out, with the properties that make the rule steady. */
class NestingModelSuite extends FunSuite {
  private def node(name: String, module: String, source: String, outer: Option[String]): Node =
    Node(name, module, Output.Scope.Main, source, 1, generated = false, exported = false, outer)

  private def unit(
      name: String,
      source: String,
      outer: Option[String] = None,
      module: String = "m",
      lines: Int = 5,
      inside: Boolean = false,
      movable: Boolean = true,
      owner: Boolean = true,
      bare: Boolean = false,
      entry: Boolean = false
  ): Nesting.Declaration =
    Nesting.Declaration(node(name, module, source, outer), unit = true, inside, lines, movable, owner, bare, entry)

  private def member(name: String, owner: String, source: String, module: String = "m", lines: Int = 2) =
    Nesting.Declaration(
      node(name, module, source, Some(owner)),
      unit = false,
      inside = false,
      lines,
      movable = true,
      owner = false,
      bare = false,
      entry = false
    )

  private def plan(declarations: Vector[Nesting.Declaration], edges: (String, String)*): Nesting.Plan =
    Nesting.plan(
      Nesting.Model(declarations, edges.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap),
      Nesting.Config()
    )

  private def moved(plan: Nesting.Plan): Map[String, Option[String]] =
    plan.moves.map(move => move.declaration.name -> move.to.owner).toMap

  test("a unit used from another module is a root, so it is never nested, and a nested one comes out") {
    val declarations = Vector(
      unit("Api", "Api.scala", module = "a"),
      unit("Helper", "Helper.scala", module = "a"),
      unit("Outer", "Outer.scala", module = "a"),
      unit("Outer.Shared", "Outer.scala", Some("Outer"), module = "a"),
      unit("Client", "Client.scala", module = "b")
    )
    val found = plan(
      declarations,
      "Client" -> "Api",
      "Client" -> "Outer.Shared",
      "Api"    -> "Helper"
    )
    assertEquals(moved(found), Map("Helper" -> Some("Api"), "Outer.Shared" -> None))
  }

  test("members in a cycle of members stay") {
    val declarations = Vector(
      unit("Lane", "Lane.scala"),
      unit("Floats", "Floats.scala"),
      member("Floats.a", "Floats", "Floats.scala"),
      member("Floats.b", "Floats", "Floats.scala")
    )
    val found = plan(declarations, "Lane" -> "Floats.a", "Floats.a" -> "Floats.b", "Floats.b" -> "Floats.a")
    assertEquals(moved(found), Map("Floats" -> Some("Lane")))
  }

  test("a unit nothing uses is the API of its build and stays at the top level") {
    val found = plan(Vector(unit("Alone", "Alone.scala"), unit("Used", "Used.scala")), "Alone" -> "Used")
    assertEquals(moved(found), Map("Used" -> Some("Alone")))
  }

  private def generate(seed: Long): Nesting.Model = {
    val random = Random(seed)
    val scale  = 1 + (seed % 3).toInt
    val tops   = scale * 3 + random.nextInt(6)
    val names  = Vector.tabulate(tops)(index => s"u$index")
    val module = names.zip(Vector.fill(tops)(if (random.nextInt(4) == 0) "b" else "a")).toMap
    val top    = Vector.tabulate(tops)(index =>
      unit(
        names(index),
        s"f${if (index > 0 && random.nextInt(4) == 0) index - 1 else index}.scala",
        module = module(names(index)),
        lines = 1 + random.nextInt(60),
        movable = random.nextInt(6) > 0,
        owner = random.nextInt(8) > 0,
        bare = random.nextInt(5) == 0,
        entry = random.nextInt(12) == 0
      )
    )
    def depth(found: Vector[Nesting.Declaration], d: Nesting.Declaration): Int =
      d.node.outer.flatMap(owner => found.find(_.name == owner)).fold(0)(depth(found, _) + 1)
    val nested = (0 until scale * 2 + random.nextInt(8)).foldLeft(top) { (found, index) =>
      val parent = found(random.nextInt(found.size))
      if (depth(found, parent) > 1) found
      else
        found :+ unit(
          s"${parent.name}.n$index",
          parent.node.source,
          Some(parent.name),
          parent.node.module,
          1 + random.nextInt(40),
          inside = random.nextBoolean(),
          movable = random.nextInt(6) > 0,
          owner = random.nextInt(8) > 0,
          bare = random.nextInt(5) == 0,
          entry = random.nextInt(14) == 0
        )
    }
    val members = Vector.tabulate(scale * 3 + random.nextInt(12)) { index =>
      val parent = nested(random.nextInt(nested.size))
      member(s"${parent.name}.m$index", parent.name, parent.node.source, parent.node.module, 1 + random.nextInt(20))
        .copy(movable = random.nextInt(6) > 0)
    }
    val all   = nested ++ members
    val order = all.map(_.name)
    val edges = Vector.tabulate(order.size) { index =>
      val targets = Vector.fill(random.nextInt(2 + scale)) {
        if (random.nextInt(5) > 0) order(index + random.nextInt(order.size - index))
        else order(random.nextInt(order.size))
      }
      order(index) -> (targets.toSet - order(index))
    }.toMap
    Nesting.Model(all, edges)
  }

  // A move into a declaration that still sits inside the mover waits for that declaration to leave, so not every
  // subset of moves is a source that can be written.
  private def apply(model: Nesting.Model, applied: Seq[Nesting.Move]): Option[Nesting.Model] = {
    val places = applied.map(move => move.declaration.name -> move.to).toMap
    val placed = model.declarations.map(d =>
      places.get(d.name).fold(d)(place => d.copy(node = d.node.copy(outer = place.owner), inside = place.inside))
    )
    val byName                                                               = placed.map(d => d.name -> d).toMap
    def top(d: Nesting.Declaration, steps: Int): Option[Nesting.Declaration] =
      if (steps > byName.size) None
      else d.node.outer.fold(Some(d))(owner => top(byName(owner), steps + 1))
    def moving(d: Nesting.Declaration): Boolean =
      places.contains(d.name) || d.node.outer.exists(owner => moving(byName(owner)))
    val homed = placed.map(d => d -> top(d, 0))
    Option.when(homed.forall(_._2.isDefined))(
      model.copy(declarations =
        homed.map((d, root) =>
          if (d.node.outer.isDefined && moving(d)) d.copy(node = d.node.copy(source = root.get.node.source)) else d
        )
      )
    )
  }

  private val configs = Vector(
    Nesting.Config(),
    Nesting.Config(depth = 3, size = 60),
    Nesting.Config(depth = 1, damping = 0, size = 1000),
    Nesting.Config(damping = 2, size = 25)
  )

  private def asked(moves: Vector[Nesting.Move]): Set[(String, Nesting.Place)] =
    moves.map(move => move.declaration.name -> move.to).toSet

  private def disagreements(seed: Long, config: Nesting.Config): Vector[String] = {
    val model = generate(seed)
    val first = Nesting.plan(model, config)
    val every = apply(model, first.moves) match {
      case Some(done) =>
        Option.when(Nesting.plan(done, config).moves.nonEmpty)(s"seed $seed $config: moves remain after applying all")
      case None => Some(s"seed $seed $config: the target is not a tree")
    }
    val parts = Vector.newBuilder[String]
    (1L to 3L).foreach { pick =>
      val random = Random(seed * 31 + pick)
      val chosen = first.moves.zip(Vector.fill(first.moves.size)(random.nextBoolean())).collect { case (move, true) =>
        move
      }
      apply(model, chosen).flatMap { done =>
        val left  = asked(first.moves) -- asked(chosen)
        val rerun = asked(Nesting.plan(done, config).moves)
        Option.when(left != rerun)(
          s"seed $seed $config applying ${chosen.map(_.declaration.name)}: expected $left, found $rerun"
        )
      }.foreach(parts += _)
    }
    every.toVector ++ parts.result()
  }

  test("applying every move leaves none, and applying some leaves the others") {
    val problems = for {
      seed   <- (1L to sys.env.get("NESTING_SEEDS").flatMap(_.toLongOption).getOrElse(400L)).toVector
      config <- configs
      found  <- disagreements(seed, config)
    } yield found
    assert(problems.isEmpty, s"${problems.size} disagreements\n${problems.take(5).mkString("\n")}")
  }

  test("generated models move enough for the subsets to differ") {
    val counts = (1L to 300L).map(seed => Nesting.plan(generate(seed), Nesting.Config()).moves.size)
    assert(counts.count(_ > 0) > 60 && counts.count(_ >= 3) > 15, counts.toString)
  }
}
