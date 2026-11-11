ThisBuild / scalaVersion := "3.9.0"
ThisBuild / bonsaiMode   := BonsaiMode.Strict

// A compile slower than copying the resources that then clears them, as an sbt 2 compile restored from the cache does.
Compile / compile := Def.uncached {
  val analysis = (Compile / compile).value
  Thread.sleep(3000)
  IO.delete((Compile / classDirectory).value / "META-INF")
  analysis
}

Seq(Compile, Test).map { configuration =>
  configuration / copyResources := Def.uncached[Seq[(File, File)]](sys.error("bonsai must not copy resources"))
}
