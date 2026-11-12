ThisBuild / scalaVersion := "3.10.0"
ThisBuild / bonsaiMode   := BonsaiMode.Relaxed

lazy val checkSources = taskKey[Unit]("Check the manifest lists the sources the compiles read and the classes they wrote")

lazy val emptied  = project.settings(Compile / sources := Nil)
lazy val skipped  = project.settings(Test / skip := true)
lazy val comments = project

lazy val root = project.in(file(".")).aggregate(emptied, skipped, comments).settings(
  checkSources := Def.uncached {
    val lines   = IO.readLines(target.value / "bonsai" / "manifest.tsv")
    val sources = lines.collect { case s"source\t$module\t$scope\t$file" => (module, scope, file.split('/').last) }
    val classes = lines.collect { case s"product\t$module\t$scope\t$file" => (module, scope, file.split('/').last) }
    val listed  = sources.map { case (module, scope, file) => s"$module $scope $file" }.sorted
    assert(listed == Seq("comments main Real.scala"), listed.mkString(", "))
    assert(classes.exists(_._3 == "Real.class"), classes.mkString(", "))
    assert(!classes.exists(_._1 != "comments"), classes.mkString(", "))
  }
)
