ThisBuild / scalaVersion := "3.10.0"
ThisBuild / bonsaiMode   := BonsaiMode.Strict

Compile / unmanagedResources / excludeFilter := HiddenFileFilter || "*.bak"

Compile / resourceGenerators += Def.task {
  val file = (Compile / resourceManaged).value / "META-INF" / "services" / "demo.Shout"
  IO.write(file, "demo.Loud\n")
  Seq(file)
}.taskValue

lazy val stray         = taskKey[Unit]("Write a hidden resource that is not valid text")
lazy val checkResource = taskKey[Unit]("Check the manifest names the resources the jar holds")

stray := {
  val services = baseDirectory.value / "src" / "main" / "resources" / "META-INF" / "services"
  IO.write(services / ".demo.Greeter.tmp", Array[Byte](-23, 10))
}

checkResource := {
  val named = IO.readLines(target.value / "bonsai" / "manifest.tsv").filter(_.startsWith("resource\t"))
    .map(_.split("\t")(2)).sorted
  assert(named == Seq("META-INF/services/demo.Greeter", "META-INF/services/demo.Shout"), named.mkString("\n"))
}
