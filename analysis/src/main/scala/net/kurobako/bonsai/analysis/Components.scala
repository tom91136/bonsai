package net.kurobako.bonsai.analysis

/** The components `References` feeds directly, and the `Recorder` for the rest. */
private[analysis] case class Components(
    symbols: Symbols,
    callSites: CallSites,
    reads: Reads,
    derivations: Derivations,
    dispatching: Dispatch,
    structure: StructureObserver,
    library: Library,
    recorder: Recorder
)
