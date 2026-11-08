ThisBuild / scalaVersion := "3.10.0"
ThisBuild / bonsaiMode   := BonsaiMode.CI

def generate(directory: File, name: String): Seq[File] = {
  val file = directory / "demo" / s"$name.scala"
  IO.write(file, s"package demo\n\nobject $name {\n  def spare: Int = 2\n}\n")
  Seq(file)
}

lazy val root = project.in(file(".")).settings(
  Compile / sourceGenerators += Def.task(generate((Compile / sourceManaged).value, "Managed")).taskValue,
  Compile / sourceGenerators += Def.task(generate(target.value / "emitted", "Emitted")).taskValue
)
