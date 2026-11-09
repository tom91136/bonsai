ThisBuild / scalaVersion := "3.9.0"
ThisBuild / bonsaiMode   := BonsaiMode.CI

def generate(directory: File, name: String, body: String = "def spare: Int = 2"): Seq[File] = {
  val file = directory / "demo" / s"$name.scala"
  IO.write(file, s"package demo\n\nobject $name {\n  $body\n}\n")
  Seq(file)
}

val violation =
  "def read(s: String): Either[String, Int] = try Right(Integer.parseInt(s)) catch { case _: Throwable => Left(s) }"

lazy val root = project.in(file(".")).settings(
  Compile / sourceGenerators += Def.uncached(Def.task(generate((Compile / sourceManaged).value, "Managed"))).taskValue,
  Compile / sourceGenerators += Def.uncached(Def.task(generate(target.value / "emitted", "Emitted"))).taskValue,
  Compile / sourceGenerators += Def.uncached(Def.task(generate((Compile / sourceManaged).value, "Lint", violation))).taskValue
)
