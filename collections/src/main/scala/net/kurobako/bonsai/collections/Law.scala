package net.kurobako.bonsai.collections

/** A law over a window of adjacent steps, as data: the methods each step may be, or one slot the `repeat`s of which
  * make a run of two or more; `applies` for what must hold for it to speak at all; `into`, the method it improves to,
  * which the cost model needs; and what it says. With a `rewrite` and `exact` holding, the outcome is that rewrite;
  * otherwise it is reported, with `report` or else the `message`, where an exact outcome that cannot be written keeps
  * the `message`. `advice` laws apply to the steps no other law took. A law that is an `idiom` is accepted at an equal
  * cost. The compiler plugin reports what the law finds, but for a `deferred` law only the exact rewrites, the rest
  * being left to the analysis; the analysis also reports what an `analysed` law finds where it saves passes or
  * intermediate collections. A step written with type arguments, or a call the compiler wrote for a for-comprehension,
  * is never rewritten, since the rewrite would drop the arguments or have no text to replace, only reported. The report
  * of a `viewable` law says that a view runs the steps in one pass. A law with a `java` form is one the javac plugin
  * reports, in the words of that form; the compiler plugin never reads it.
  */
private[collections] case class Law(
    id: String,
    pattern: Vector[Set[String]],
    into: String,
    message: Hit => String,
    applies: Hit => Boolean = _ => true,
    rewrite: Option[(Hit, String) => String] = None,
    exact: Hit => Boolean = _ => true,
    report: Option[Hit => String] = None,
    repeat: Boolean = false,
    advice: Boolean = false,
    idiom: Boolean = false,
    deferred: Boolean = false,
    analysed: Boolean = false,
    viewable: Boolean = false,
    java: Option[JavaForm] = None
) {

  /** The steps of `chain` this law matches from `from`, all of which `free` allows, as a hit that is accepted. */
  def hit(chain: Chain, from: Int, free: Int => Boolean): Option[Hit] =
    window(chain, from, free)
      .filter(_.forall(_.receiver.library))
      .map { steps =>
        val replacement = Step(into, Vector.empty, steps.head.receiver, steps.last.result)
        Hit(chain, from, steps, Ops.cost(steps), Ops.cost(replacement))
      }
      .filter { hit =>
        applies(hit) && (if (idiom) Cost.neverWorse(hit.before, hit.after) else Cost.improves(hit.before, hit.after))
      }

  def outcome(hit: Hit): Outcome = {
    val saving  = analysed && Cost.improves(hit.before, hit.after)
    val written = java.filter(_.applies(hit)).map { form =>
      Java(
        form.message(hit),
        form.rewrite.filter(_ => rewritable(hit)).map(render => receiver => render(hit, receiver))
      )
    }
    rewrite match {
      case Some(render) if exact(hit) && rewritable(hit) =>
        Outcome.Rewrite(
          id,
          hit.from,
          hit.until,
          message(hit),
          receiver => render(hit, receiver),
          analysis = saving,
          java = written
        )
      case _ =>
        Outcome.Report(
          id,
          hit.from,
          hit.until,
          if (rewrite.isDefined && exact(hit)) message(hit) else report.getOrElse(message)(hit),
          lint = !deferred,
          analysis = saving || deferred,
          viewable = viewable,
          pure = viewable && hit.steps.forall(_.args.forall(_.pure)),
          java = written
        )
    }
  }

  private def rewritable(hit: Hit): Boolean = !hit.steps.exists(step => step.typed || step.desugared)

  private def window(chain: Chain, from: Int, free: Int => Boolean): Option[Vector[Step]] =
    if (repeat) {
      val run = chain.steps.drop(from).zipWithIndex.takeWhile((step, k) => free(from + k) && pattern.head(step.method))
      val written  = run.takeWhile(!_._1.desugared)
      val selected = if (written.sizeIs >= 2) written else run
      Option.when(selected.size >= 2)(selected.map(_._1))
    } else {
      val steps = chain.steps.slice(from, from + pattern.size)
      Option.when(steps.size == pattern.size &&
        steps.indices.forall(k => free(from + k) && pattern(k)(steps(k).method)))(
        steps
      )
    }
}
