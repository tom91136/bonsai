package net.kurobako.bonsai.collections

/** The laws of collection chains, which the normaliser applies to a chain to find cheaper forms of it. */
object Laws {

  /** The ids of the laws, which the tools that report what they find name their checks by. */
  val ids: Vector[String] = Catalogue.laws.map(_.id).distinct

  /** The ids of the laws that read as Java, which the javac plugin reports under. */
  val javaIds: Vector[String] = Catalogue.laws.filter(_.java.isDefined).map(_.id).distinct

  private case class Found(hit: Hit, order: Int, outcome: Outcome, speaks: Boolean)

  private type Index = Map[String, Vector[(Law, Int)]]

  private def byFirstMethod(laws: Vector[(Law, Int)]): Index =
    laws.flatMap((law, order) => law.pattern.head.toVector.map(_ -> (law, order))).groupMap(_._1)(_._2)

  private val (adviceLaws, exactLaws) = Catalogue.laws.zipWithIndex.partition(_._1.advice)
  private val passes                  = Vector(byFirstMethod(exactLaws), byFirstMethod(adviceLaws))

  /** What the laws find for `chain`, as non-overlapping outcomes in the order of their steps. */
  def outcomes(chain: Chain): Vector[Outcome] = {
    val (_, chosen) = passes.foldLeft((Set.empty[Int], Vector.empty[Found])) { case ((taken, chosen), index) =>
      val picked = choose(find(chain, index, !taken(_)))
      (taken ++ picked.flatMap(found => found.hit.from until found.hit.until), chosen ++ picked)
    }
    chosen.filter(_.speaks).sortBy(_.hit.from).map(_.outcome)
  }

  private def find(chain: Chain, index: Index, free: Int => Boolean): Vector[Found] =
    for {
      (step, from) <- chain.steps.zipWithIndex
      (law, order) <- index.getOrElse(step.method, Vector.empty)
      hit          <- law.hit(chain, from, free)
    } yield Found(hit, order, law.outcome(hit), law.speaks(hit))

  private def choose(found: Vector[Found]): Vector[Found] = {
    val (_, chosen) = found.sortBy(f => (-f.hit.steps.size, f.hit.from, f.order))
      .foldLeft((Set.empty[Int], Vector.empty[Found])) { case ((covered, chosen), next) =>
        val span = next.hit.from until next.hit.until
        if (span.exists(covered)) (covered, chosen) else (covered ++ span, chosen :+ next)
      }
    chosen
  }
}
