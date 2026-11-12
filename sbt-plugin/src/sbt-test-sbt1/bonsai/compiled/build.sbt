ThisBuild / scalaVersion := "3.10.0"
ThisBuild / bonsaiMode   := BonsaiMode.Relaxed

lazy val checkSources = taskKey[Unit]("Check the manifest lists the sources the compiles read and the classes they wrote")

lazy val emptied  = project.settings(Compile / sources := Nil)
lazy val skipped  = project.settings(Test / skip := true)
lazy val comments = project

lazy val root = project.in(file(".")).aggregate(emptied, skipped, comments).settings(
  checkSources := {
    val lines   = IO.readLines(target.value / "bonsai" / "manifest.tsv")
    def entries(kind: String) = lines.map(_.split("\t")).collect {
      case Array(`kind`, module, scope, file) => (module, scope, file.split('/').last)
    }
    val sources = entries("source")
    val classes = entries("product")
    val listed  = sources.map { case (module, scope, file) => s"$module $scope $file" }.sorted
    assert(listed == Seq("comments main Real.scala"), listed.mkString(", "))
    assert(classes.exists(_._3 == "Real.class"), classes.mkString(", "))
    assert(!classes.exists(_._1 != "comments"), classes.mkString(", "))
  }
)
