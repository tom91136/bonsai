ThisBuild / scalaVersion := "3.9.0"
ThisBuild / bonsaiMode   := BonsaiMode.Dev

lazy val danglingLink = taskKey[java.nio.file.Path]("Link a semanticdb to a cache entry that no longer exists")
lazy val checkGone    = taskKey[Unit]("Check the dangling link is gone")

danglingLink := Def.uncached {
  val link = (Compile / semanticdbTargetRoot).value.toPath.resolve("Gone.scala.semanticdb")
  java.nio.file.Files.createDirectories(link.getParent)
  java.nio.file.Files.createSymbolicLink(link, link.resolveSibling("evicted"))
}

checkGone := Def.uncached {
  val link = (Compile / semanticdbTargetRoot).value.toPath.resolve("Gone.scala.semanticdb")
  assert(!java.nio.file.Files.isSymbolicLink(link), s"$link is still there")
}
