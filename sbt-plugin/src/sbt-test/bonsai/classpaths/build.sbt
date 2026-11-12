ThisBuild / scalaVersion := "3.10.0"
ThisBuild / bonsaiMode   := BonsaiMode.Relaxed

lazy val checkClasspaths = taskKey[Unit]("Check the manifest holds the classpath each scope compiled with")

val xml = "org.scala-lang.modules" %% "scala-xml"

lazy val split = project.settings(libraryDependencies ++= Seq(xml % "2.1.0", xml % "2.4.0" % Test))

lazy val trimmed = project.settings(
  libraryDependencies += xml % "2.1.0",
  Compile / dependencyClasspath := (Compile / dependencyClasspath).value.filterNot(_.data.name.contains("scala-xml"))
)

lazy val root = project.in(file(".")).aggregate(split, trimmed).settings(
  checkClasspaths := Def.uncached {
    val lines = IO.readLines(target.value / "bonsai" / "manifest.tsv")
    val jars  = lines.collect { case s"classpath\t$module\t$scope\t$file" => s"$module $scope ${file.split('/').last}" }
    def xmls(module: String, scope: String) = jars.filter(_.startsWith(s"$module $scope scala-xml")).map(_.split(' ').last)
    assert(xmls("split", "main") == Seq("scala-xml_3-2.1.0.jar"), jars.mkString(", "))
    assert(xmls("split", "test") == Seq("scala-xml_3-2.4.0.jar"), jars.mkString(", "))
    assert(xmls("trimmed", "main").isEmpty, jars.mkString(", "))
    assert(xmls("trimmed", "test") == Seq("scala-xml_3-2.1.0.jar"), jars.mkString(", "))
  }
)
