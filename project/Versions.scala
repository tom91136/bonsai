/** The versions the build and the meta-build share. */
object Versions {

  /** The version of the sbt-scalafix plugin, which the scalafix rules run with. */
  val scalafix: String = "0.14.9"

  /** The newest release of sbt-scalafix also published under the name sbt before 1.9 asks for, and the oldest the rules
    * run on, so the one they build against.
    */
  val scalafixSbt1: String = "0.14.3"

  /** The oldest release of Scala 3 whose compiler takes the `-Wunused:all` that RemoveUnused reads. */
  val removeUnusedFrom: String = "3.3.4"

  /** The lines of Scala 3 whose presentation compiler ExplicitResultTypes cannot load from this scalafix. */
  val explicitResultTypesExcluded: Seq[String] = Seq("3.4")
}
