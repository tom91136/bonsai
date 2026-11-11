package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path

object QuietProbe {
  def main(args: Array[String]): Unit = Quiet.attempt {
    println("held out")
    Console.err.println("held err")
    // scalafix:off ForbiddenAPIs.forbidden-api
    System.err.println("held trace")
    // scalafix:on ForbiddenAPIs.forbidden-api
    Files.writeString(Path.of(args(1)), "ready"): Unit
    if (args(0) == "exit") sys.exit(0) else Thread.sleep(Long.MaxValue)
  }(_ => true)
}
