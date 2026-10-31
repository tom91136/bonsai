package net.kurobako.bonsai.scalafix

import metaconfig.Configured
import scala.meta.*
import scalafix.v1.*

object PackageBoundary {
  case class Boundary(
      sources: scala.util.matching.Regex,
      exempt: Option[scala.util.matching.Regex],
      forbidden: List[String],
      message: String
  )
}

/** Sources matching a boundary's `sources` pattern must not reference symbols under its `forbidden` prefixes, unless
  * they also match its `exempt` pattern; symbols use SemanticDB form such as `org/example/asm/`.
  */
class PackageBoundary(boundaries: List[PackageBoundary.Boundary]) extends SemanticRule("PackageBoundary") {
  def this() = this(Nil)

  override def withConfiguration(config: Configuration): Configured[Rule] = Configured.Ok(
    PackageBoundary(settings.objects(settings.section(config.conf, "PackageBoundary"), "boundaries").map { fields =>
      PackageBoundary.Boundary(
        settings.string(fields, "sources").getOrElse("").r,
        settings.string(fields, "exempt").filter(_.nonEmpty).map(_.r),
        settings.strings(fields, "forbidden"),
        settings.string(fields, "message").getOrElse("this source must not reference that package")
      )
    })
  )

  override def fix(using document: SemanticDocument): Patch = {
    val source = definitions.path(document.input)
    boundaries.view.filter(boundary =>
      boundary.sources.findFirstIn(source).nonEmpty && boundary.exempt.forall(_.findFirstIn(source).isEmpty)
    ).flatMap { boundary =>
      document.tree.collect {
        case name: Name if boundary.forbidden.exists(name.symbol.value.startsWith(_)) => name
      }.groupBy(_.pos.startLine).values.map(names => lint("package-boundary", boundary.message, names.head.pos))
    }.toList.asPatch
  }
}
