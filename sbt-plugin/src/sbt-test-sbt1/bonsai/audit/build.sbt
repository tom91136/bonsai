ThisBuild / scalaVersion := "3.10.0"
ThisBuild / bonsaiMode   := BonsaiMode.Audit

val jdk: File = {
  val home = file(".").getAbsoluteFile / "jdk"
  val java = home / "bin" / "java"
  IO.write(
    java,
    s"""#!/bin/sh
       |case "$$1" in -version) echo 'openjdk version "21.0.1"' >&2; exit 0 ;; esac
       |report="$$(dirname "$$0")/../report"
       |"${sys.props("java.home")}/bin/java" "$$@" > "$$report.part"
       |code=$$?
       |cat "$$report.part"
       |mv "$$report.part" "$$report"
       |exit $$code
       |""".stripMargin
  )
  java.setExecutable(true)
  home
}

ThisBuild / bonsaiAnalysisJavaHome := jdk

lazy val reported = inputKey[Unit]("Check the last report holds each level:rule given, and none given as !level:rule")
lazy val stamp    = taskKey[Unit]("Note when each record of findings was written")
lazy val kept     = inputKey[Unit]("Check the records of the sources given were not written since stamp, and those of the others were")

def tree(directory: File): Seq[File] =
  Option(directory.listFiles).toSeq.flatten.flatMap(file => if (file.isDirectory) tree(file) else Seq(file))

def written(base: File): Map[String, Long] =
  tree(base / "target").filter(file => file.getParent.endsWith("META-INF/bonsai/findings") && file.getName.endsWith(".tsv"))
    .map(file => IO.readLines(file).head.split('\t')(1) -> file.lastModified).toMap

lazy val root = project.in(file(".")).settings(
  scalacOptions += "-Werror",
  reported := {
    val lines = IO.readLines(jdk / "report")
    Def.spaceDelimited().parsed.foreach { expectation =>
      val absent             = expectation.startsWith("!")
      val Array(level, rule) = expectation.stripPrefix("!").split(':')
      val found              = lines.exists(_.startsWith(s"$level\t$rule\t"))
      assert(found != absent, s"$expectation: ${lines.mkString("\n")}")
    }
  },
  stamp := {
    IO.writeLines(baseDirectory.value / "stamps", written(baseDirectory.value).toSeq.map { case (k, v) => s"$k\t$v" })
  },
  kept := {
    val before = IO.readLines(baseDirectory.value / "stamps").map(_.split('\t')).map(f => f(0) -> f(1).toLong).toMap
    val after  = written(baseDirectory.value)
    val named  = Def.spaceDelimited().parsed.toSet
    after.foreach { case (source, time) =>
      val unchanged = before.get(source).contains(time)
      assert(unchanged == named.exists(source.endsWith), s"$source: ${before.get(source)} then $time")
    }
  }
)
