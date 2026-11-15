import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, StandardOpenOption}
import _root_.scalafix.interfaces.{ScalafixDiagnostic, ScalafixMainCallback}

ThisBuild / scalaVersion := "3.9.0"
ThisBuild / bonsaiMode := BonsaiMode.Relaxed

ThisBuild / scalafixCallback := {
  val report = (ThisBuild / baseDirectory).value.toPath.resolve("diagnostics.txt")
  new ScalafixMainCallback {
    def reportDiagnostic(diagnostic: ScalafixDiagnostic): Unit =
      Files.write(report, s"${diagnostic.message}\n".getBytes(UTF_8), StandardOpenOption.CREATE, StandardOpenOption.APPEND)
  }
}
