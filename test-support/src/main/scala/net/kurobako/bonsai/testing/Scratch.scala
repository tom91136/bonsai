package net.kurobako.bonsai.testing

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import scala.util.Using

object Scratch {

  private val root = {
    val directory = Files.createTempDirectory("bonsai-tests-")
    sys.runtime.addShutdownHook(Thread(() => delete(directory)))
    directory
  }

  def directory(prefix: String): Path = Files.createTempDirectory(root, prefix)

  def file(prefix: String, suffix: String): Path = Files.createTempFile(root, prefix, suffix)

  def using[A](prefix: String)(body: Path => A): A = {
    val path = directory(prefix)
    try body(path)
    finally delete(path)
  }

  private def delete(path: Path): Unit =
    Using.resource(Files.walk(path))(_.toList.asScala.reverse.foreach(Files.delete(_)))
}
