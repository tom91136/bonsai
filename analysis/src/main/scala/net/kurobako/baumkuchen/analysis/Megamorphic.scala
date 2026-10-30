package net.kurobako.baumkuchen.analysis

/** Virtual calls inside loops whose targets have more implementations than HotSpot inlines at one call site. */
object Megamorphic {

  /** A method calling `callee` inside a loop, with the classes implementing it. */
  case class Site(
      caller: String,
      node: String,
      callee: String,
      implementations: Vector[String],
      suppressed: Set[String]
  )

  private val Shown = 4

  def findings(sites: Vector[Site], nodes: Map[String, Node]): Vector[Finding] = sites.flatMap { site =>
    Finding.at(
      nodes,
      site.node,
      Finding.Rule.MegamorphicCall,
      s"${Finding.short(site.caller)} calls ${site.callee} in a loop, which has " +
        s"${site.implementations.size} implementations (${Finding.listed(site.implementations.sorted, Shown)}); " +
        "HotSpot inlines at most two receiver types at a call site",
      suppressed = site.suppressed
    )
  }
}
