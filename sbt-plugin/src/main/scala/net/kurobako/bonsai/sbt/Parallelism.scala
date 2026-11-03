package net.kurobako.bonsai.sbt

import sbt.Tags

/** The parallelism of the build, which the analysis may use for the stages that touch no compiler state. */
private[sbt] object Parallelism {

  /** The most tasks the `restrictions` let sbt run at once, up to `processors`: the largest count they allow, with one
    * as the least, as sbt always runs a task alone.
    */
  def of(restrictions: Seq[Tags.Rule], processors: Int = Runtime.getRuntime.availableProcessors): Int = {
    val allowed = Tags.predicate(restrictions)
    (processors to 2 by -1).find(count => allowed(Map(Tags.All -> count))).getOrElse(1)
  }
}
