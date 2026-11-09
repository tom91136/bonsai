ThisBuild / scalaVersion := "3.9.0"
ThisBuild / bonsaiMode   := BonsaiMode.Relaxed

// as the documentation tools of a build pin the Scala libraries to the version it compiles with
ThisBuild / dependencyOverrides ++= Seq(
  "org.scala-lang" % "scala-library"    % "3.9.0",
  "org.scala-lang" %% "scala3-library"  % "3.9.0",
  "org.scala-lang" %% "scala3-compiler" % "3.9.0",
  "org.scala-lang" %% "tasty-core"      % "3.9.0"
)
