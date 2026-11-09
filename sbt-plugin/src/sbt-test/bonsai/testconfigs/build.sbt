ThisBuild / scalaVersion   := "3.9.0"
ThisBuild / bonsaiMode := BonsaiMode.Strict
ThisBuild / bonsaiNonfatal += "test-only-member"

lazy val IT     = config("it").extend(Test)
lazy val Deep   = config("deep").extend(IT)
lazy val Shared = config("shared").extend(Test)
lazy val Bare   = config("bare").extend(Test)
lazy val Plain  = config("plain").extend(Runtime)

lazy val checkOutputs = taskKey[Unit]("Check the outputs and sources the manifest lists")

lazy val root = project.in(file("."))
  .configs(IT, Deep, Shared, Bare, Plain)
  .settings(
    inConfig(IT)(Defaults.testSettings),
    inConfig(Deep)(Defaults.testSettings),
    inConfig(Shared)(Defaults.testSettings),
    inConfig(Plain)(Defaults.testSettings),
    Shared / scalaSource := (IT / scalaSource).value,
    checkOutputs         := Def.uncached {
      val lines   = IO.readLines(target.value / "bonsai" / "manifest.tsv")
      assert(lines.count(_.startsWith("jdk\t")) == 1, lines.mkString(", "))
      val outputs = lines.collect { case s"output\t$_\t$scope\t$directory" => scope -> file(directory).getName }
      val classes = outputs.filter(_._1 == "test").map(_._2).sorted
      assert(outputs.count(_._1 == "main") == 1, outputs.mkString(", "))
      assert(classes.size == 3 && classes.distinct.size == 3, outputs.mkString(", "))
      val sources = lines.collect { case s"source\t$_\t$scope\t$file" => scope -> file }
      assert(
        sources.count(_._2.endsWith("It.scala")) == 1 && sources.count(_._2.endsWith("Deep.scala")) == 1,
        sources.mkString(", ")
      )
      assert(sources.filter(_._2.endsWith("It.scala")).forall(_._1 == "test"), sources.mkString(", "))
    }
  )
