package net.kurobako.bonsai.sbt

import sbt.*
import scala.collection.mutable
import scala.sys.process.Process
import scala.sys.process.ProcessLogger
import scala.util.Try

/** The JDK the analysis runs on, which is not the one the build targets: the analysis links a compiler that needs JDK
  * 17, while the build may target 8.
  */
private[sbt] object AnalysisJdk {

  val Minimum = 17

  private val Legacy = """version "1\.(\d+)[^"]*"""".r.unanchored
  private val Modern = """version "(\d+)[^"]*"""".r.unanchored

  /** The major version a `java -version` output names, where `1.8.0_402` is 8. */
  def major(output: String): Option[Int] = output match {
    case Legacy(minor) => Some(minor.toInt)
    case Modern(major) => Some(major.toInt)
    case _             => None
  }

  /** The major version of the java binary under `home`, or none when it does not run. */
  def probe(home: File): Option[Int] = {
    val output = mutable.ArrayBuffer.empty[String]
    val java   = (home / "bin" / "java").getAbsolutePath
    Try(Process(Seq(java, "-version")).!(ProcessLogger(line => output += line))).toOption
      .flatMap(_ => major(output.mkString("\n")))
  }

  /** The home, unless its java binary is older than the minimum or its version cannot be read. */
  def select(home: File, probe: File => Option[Int]): Either[String, File] = {
    val advice = s"set bonsaiAnalysisJavaHome to a JDK $Minimum or later"
    probe(home) match {
      case Some(version) if version >= Minimum => Right(home)
      case Some(version)                       =>
        Left(
          s"bonsai: the analysis needs JDK $Minimum or later but bonsaiAnalysisJavaHome is $home, JDK $version; $advice"
        )
      case None =>
        Left(s"bonsai: cannot read the version of the java binary in bonsaiAnalysisJavaHome $home; $advice")
    }
  }
}
