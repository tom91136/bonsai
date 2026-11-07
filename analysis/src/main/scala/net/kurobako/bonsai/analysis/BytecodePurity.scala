package net.kurobako.bonsai.analysis

/** The purity model of a build whose code has no TASTy to read, from the bytecode summaries of its classes. */
private[analysis] object BytecodePurity {

  /** The methods of the classes of `nodes`, which name the types of the build, with their verdicts. */
  def model(summaries: BytecodeEffects.Summaries, nodes: Map[String, Node]): Purity.Model =
    Purity.Model(
      Vector.empty,
      Vector.empty,
      Vector.empty,
      summaries.entries.flatMap { entry =>
        val owner = entry.method.owner.replace('/', '.').replace('$', '.').stripSuffix(".")
        nodes.get(owner).filter(_ => !entry.synthetic && entry.method.name != "<clinit>").map { node =>
          val ordered = Purity.ranked(entry.summary.certain)
          entry.key -> Purity.Method(
            owner,
            Purity.verdictOf(ordered),
            Purity.observedOf(ordered),
            entry.role,
            node.suppressed
          )
        }
      }.toMap,
      library = 0
    )
}
