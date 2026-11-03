package net.kurobako.bonsai.collections

object OptionFold {
  val Default: OptionFold = Fold

  def named(key: String): Option[OptionFold] = values.find(_.key == key)
}

/** The spelling of the default of an option that the laws prefer: `o.fold(d)(f)` or `o.map(f).getOrElse(d)`. */
enum OptionFold(val key: String) {
  case Fold         extends OptionFold("fold")
  case MapGetOrElse extends OptionFold("map-get-or-else")
}
