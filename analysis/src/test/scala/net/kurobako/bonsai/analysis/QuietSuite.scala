package net.kurobako.bonsai.analysis

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.PrintStream
import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.TimeUnit
import munit.FunSuite
import net.kurobako.bonsai.testing.*
import scala.concurrent.duration.*

class QuietSuite extends FunSuite {
  private val Word = "café über"

  private val Bytes = Word.getBytes(ISO_8859_1)

  // scalafix:off ForbiddenAPIs.forbidden-api
  private def latin1[A](body: => A): (A, Array[Byte], Array[Byte]) = {
    val out    = ByteArrayOutputStream()
    val err    = ByteArrayOutputStream()
    val before = (System.out, System.err)
    val stdout = PrintStream(out, true, ISO_8859_1)
    val stderr = PrintStream(err, true, ISO_8859_1)
    System.setOut(stdout)
    System.setErr(stderr)
    try {
      val value = Console.withOut(stdout)(Console.withErr(stderr)(body))
      (value, out.toByteArray, err.toByteArray)
    } finally {
      System.setOut(before._1)
      System.setErr(before._2)
    }
  }

  test("an attempt that stands prints in the charset of the stream it was written to") {
    val (_, out, err) = latin1(Quiet.attempt {
      println(Word)
      Console.err.println(Word)
      System.out.println(Word)
      System.err.println(Word)
    }(_ => false))
    assertEquals(out.toSeq, (Bytes ++ "\n".getBytes(ISO_8859_1) ++ Bytes ++ "\n".getBytes(ISO_8859_1)).toSeq)
    assertEquals(err.toSeq, (Bytes ++ "\n".getBytes(ISO_8859_1) ++ Bytes ++ "\n".getBytes(ISO_8859_1)).toSeq)
  }

  test("output written after an attempt keeps the charset of the stream, as the attempt's did") {
    val (_, out, err) = latin1 {
      Quiet.attempt(())(_ => false)
      System.out.print(Word)
      System.err.print(Word)
      Console.err.print(Word)
    }
    assertEquals(out.toSeq, Bytes.toSeq)
    assertEquals(err.toSeq, (Bytes ++ Bytes).toSeq)
  }
  // scalafix:on ForbiddenAPIs.forbidden-api

  private def classpath: String =
    Seq(QuietProbe.getClass, Quiet.getClass, Console.getClass, scala.runtime.LazyVals.getClass)
      .map(found => Path.of(found.getProtectionDomain.getCodeSource.getLocation.toURI).toString).distinct
      .mkString(File.pathSeparator)

  private def probed(mode: String, stop: Process => Unit): (String, String) = Scratch.using("bonsai-") { directory =>
    val ready = directory.resolve("ready")
    val out   = directory.resolve("out")
    val err   = directory.resolve("err")
    val java  = Path.of(sys.props("java.home"), "bin", "java").toString
    val child = ProcessBuilder(
      java,
      "-cp",
      classpath,
      "net.kurobako.bonsai.analysis.QuietProbe",
      mode,
      ready.toString
    ).redirectOutput(out.toFile).redirectError(err.toFile).start()
    try {
      val deadline = 2.minutes.fromNow
      while (!Files.exists(ready) && child.isAlive && deadline.hasTimeLeft()) Thread.sleep(50)
      stop(child)
      assert(child.waitFor(2, TimeUnit.MINUTES))
      (Files.readString(out, UTF_8), Files.readString(err, UTF_8))
    } finally child.destroyForcibly(): Unit
  }

  test("the output an attempt holds back reaches the user when the process is terminated") {
    val (out, err) = probed("wait", _.destroy())
    assertEquals((out.linesIterator.toSeq, err.linesIterator.toSeq), (Seq("held out"), Seq("held err", "held trace")))
  }

  test("the output an attempt holds back reaches the user when the process exits during it") {
    val (out, err) = probed("exit", _ => ())
    assertEquals((out.linesIterator.toSeq, err.linesIterator.toSeq), (Seq("held out"), Seq("held err", "held trace")))
  }
}
