package net.kurobako.bonsai.sbt

/** Whether other builds use a project's public API. */
private[sbt] object Library {

  /** The configuration decides when it names the project. Otherwise a project skipped from publishing is no library,
    * and a published one is when it declares licences, developers or a homepage, as an artifact for others does, or has
    * no main class.
    */
  def detect(configured: Option[Boolean], skipped: Boolean, declared: Boolean, mainClasses: Seq[String]): Boolean =
    configured.getOrElse(!skipped && (declared || mainClasses.isEmpty))
}
