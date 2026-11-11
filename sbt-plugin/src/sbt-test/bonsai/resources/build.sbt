ThisBuild / scalaVersion := "3.9.0"
ThisBuild / bonsaiMode   := BonsaiMode.Strict

// A compile slower than copying the resources that then clears them, as an sbt 2 compile restored from the cache does.
Compile / compile := Def.uncached {
  val analysis = (Compile / compile).value
  Thread.sleep(3000)
  IO.delete((Compile / classDirectory).value / "META-INF")
  analysis
}

Compile / unmanagedResources / excludeFilter := HiddenFileFilter || "*.bak"

Compile / resourceGenerators += Def.uncached(Def.task {
  val file = (Compile / resourceManaged).value / "META-INF" / "services" / "demo.Shout"
  IO.write(file, "demo.Loud\n")
  Seq(file)
}).taskValue

lazy val stray         = taskKey[Unit]("Write a hidden resource that is not valid text")
lazy val checkResource = taskKey[Unit]("Check the manifest names the resources the jar holds")

stray := Def.uncached {
  val services = baseDirectory.value / "src" / "main" / "resources" / "META-INF" / "services"
  IO.write(services / ".demo.Greeter.tmp", Array[Byte](-23, 10))
}

checkResource := Def.uncached {
  val named = IO.readLines(target.value / "bonsai" / "manifest.tsv").filter(_.startsWith("resource\t"))
    .map(_.split("\t")(2)).sorted
  assert(named == Seq("META-INF/services/demo.Greeter", "META-INF/services/demo.Shout"), named.mkString("\n"))
}

Seq(Compile, Test).map { configuration =>
  configuration / copyResources := Def.uncached[Seq[(File, File)]](sys.error("bonsai must not copy resources"))
}
