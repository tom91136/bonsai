package net.kurobako.bonsai.sbt

/** The compiler options the plugin adds to the ones the build gives, judged by the options as they stand once the build
  * has given them all.
  */
private[sbt] object CompilerOptions {
  private val Fatal  = Set("-Werror", "-Xfatal-warnings")
  private val Unused = "-(Wunused|Ywarn-unused|Wall).*"

  /** The options with which RemoveUnused asks the compiler for unused warnings. */
  val UnusedOptions: Seq[String] = BonsaiArtifacts.builtin.filter(_.startsWith("-"))

  /** Whether the compiler fails on a warning. */
  def fatal(own: Seq[String]): Boolean = own.exists(Fatal)

  /** The options RemoveUnused reads, with the demotions that keep fatal warnings from failing the compile on what the
    * rule fixes. A build that asks for unused warnings itself gets none, and each option is added once, as the
    * compilers of some lines reject a repeated one.
    */
  def unused(own: Seq[String], disabled: Set[String], scala: String): Seq[String] =
    if (disabled("RemoveUnused") || !ScalaVersions.removeUnused(scala) || own.exists(_.matches(Unused))) Nil
    else UnusedOptions.filterNot(own.contains)

  /** The options that route the findings of the compiler plugin, whatever the build does with compiler warnings. In the
    * strict mode every finding fails except those of the checks in `warnings`. Outside it none does, so a build that
    * fails on warnings gets them all demoted.
    */
  def routed(own: Seq[String], strict: Boolean, warnings: Set[String], scala: String): Seq[String] =
    if (ScalaVersions.plugin(scala).isEmpty) Nil
    else if (strict) tagged("[a-z0-9-]+", "e") +: (warnings & BonsaiArtifacts.checks).toSeq.sorted.map(tagged(_, "i"))
    else if (fatal(own)) Seq(tagged("[a-z0-9-]+", "i"))
    else Nil

  // The compiler lets the last -Wconf that matches decide, so these go before the options of the build, whose own
  // -Wconf can then override them. A regex of the option cannot hold a colon, which the compiler splits it on, so the
  // tag matches any character beside the id.
  private def tagged(id: String, action: String): String = s"-Wconf:msg=\\[bonsai.$id\\]:$action"
}
