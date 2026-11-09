ThisBuild / scalaVersion := "3.10.0"
ThisBuild / bonsaiMode   := BonsaiMode.Dev

lazy val legacy = taskKey[Unit]("Check sbt-scalafix is a release that sbt before 1.9 resolves")

legacy := {
  val wanted = sys.props("bonsai.scalafix.sbt1")
  assert(_root_.scalafix.sbt.BuildInfo.scalafixVersion == wanted, s"expected scalafix $wanted")
}
