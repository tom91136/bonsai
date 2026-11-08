package net.kurobako.bonsai.sbt

import java.nio.file.Files
import sbt.*

class AnalysisJdkSuite extends munit.FunSuite {

  private val home = file("/jdk/home")

  private def probe(versions: (File, Int)*): File => Option[Int] = versions.toMap.get

  test("the major version reads old and new version strings") {
    assertEquals(AnalysisJdk.major("""openjdk version "1.8.0_402""""), Some(8))
    assertEquals(AnalysisJdk.major("""openjdk version "11.0.2" 2019-01-15"""), Some(11))
    assertEquals(AnalysisJdk.major("""openjdk version "17" 2021-09-14"""), Some(17))
    assertEquals(AnalysisJdk.major("""openjdk version "26-ea" 2026-03-17"""), Some(26))
    assertEquals(AnalysisJdk.major("""java version "9-internal""""), Some(9))
  }

  test("the major version skips what the launcher prints before it") {
    val output =
      "Picked up JAVA_TOOL_OPTIONS: -Xmx1g\nopenjdk version \"21.0.1\" 2023-10-17\nOpenJDK Runtime Environment"
    assertEquals(AnalysisJdk.major(output), Some(21))
  }

  test("output without a version has no major version") {
    assertEquals(AnalysisJdk.major("not a java"), None)
  }

  test("a JDK of 17 or later is selected") {
    assertEquals(AnalysisJdk.select(home, probe(home -> 17)), Right(home))
    assertEquals(AnalysisJdk.select(home, probe(home -> 25)), Right(home))
  }

  test("a JDK older than 17 fails naming the setting, the home and the version") {
    val message = AnalysisJdk.select(home, probe(home -> 11)).left.getOrElse("")
    assert(message.contains("bonsaiAnalysisJavaHome"), message)
    assert(message.contains(home.toString), message)
    assert(message.contains("JDK 11"), message)
  }

  test("a JDK whose version cannot be read fails naming the setting and the home") {
    val message = AnalysisJdk.select(home, probe()).left.getOrElse("")
    assert(message.contains("bonsaiAnalysisJavaHome"), message)
    assert(message.contains(home.toString), message)
  }

  test("the probe reads the version of the java binary of a home") {
    val jdk = Files.createTempDirectory("jdk").toFile
    val bin = jdk / "bin"
    bin.mkdirs()
    val java = bin / "java"
    Files.write(java.toPath, "#!/bin/sh\necho 'openjdk version \"1.8.0_402\"' >&2\n".getBytes("UTF-8"))
    java.setExecutable(true)
    assertEquals(AnalysisJdk.probe(jdk), Some(8))
  }

  test("the probe finds nothing in a home without a java binary") {
    assertEquals(AnalysisJdk.probe(Files.createTempDirectory("jdk").toFile), None)
  }
}
