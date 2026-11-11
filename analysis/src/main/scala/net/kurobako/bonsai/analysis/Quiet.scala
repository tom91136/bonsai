package net.kurobako.bonsai.analysis

import java.io.OutputStream
import java.io.PrintStream
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

/** Holds back what an attempt prints, to show it only when the attempt stands, or when the process ends first. Scala's
  * console is scoped to the attempt, and the platform's streams, which the compiler writes stack traces to, are routed
  * by thread, so a thread the attempt starts is held back too and no other thread is. The text travels as UTF-8 and is
  * printed through the stream it was written to, which encodes it as that stream does.
  */
private[analysis] object Quiet {

  private class Tape {
    private val chunks = AtomicReference[Option[Vector[(PrintStream, String)]]](Some(Vector.empty))

    def record(target: PrintStream, text: String): Boolean =
      chunks.updateAndGet(_.map(_ :+ (target -> text))).isDefined

    def close(): Vector[(PrintStream, String)] = chunks.getAndSet(None).getOrElse(Vector.empty)
  }

  private def replay(chunks: Vector[(PrintStream, String)]): Unit = {
    chunks.foreach((target, text) => target.print(text))
    chunks.map(_._1).distinct.foreach(_.flush())
  }

  private class Gate(tape: () => Option[Tape], target: PrintStream) extends OutputStream {
    def write(byte: Int): Unit = write(Array(byte.toByte))

    override def write(bytes: Array[Byte], off: Int, len: Int): Unit = {
      val text = String(bytes, off, len, UTF_8)
      if (!tape().exists(_.record(target, text))) replay(Vector(target -> text))
    }

    override def flush(): Unit = target.flush()
  }

  private class Routed(target: PrintStream) extends PrintStream(Gate(() => Option(current.get), target), true, UTF_8)

  private val current = InheritableThreadLocal[Tape]()

  private val open = ConcurrentHashMap.newKeySet[Tape]()

  sys.addShutdownHook(open.forEach(tape => replay(tape.close()))): Unit

  // scalafix:off ForbiddenAPIs.forbidden-api
  private def routing(): Unit = {
    if (!System.out.isInstanceOf[Routed]) System.setOut(Routed(System.out))
    if (!System.err.isInstanceOf[Routed]) System.setErr(Routed(System.err))
  }
  // scalafix:on ForbiddenAPIs.forbidden-api

  /** Runs `body`, and prints what it printed once it ends, unless it returns a result that `discard` holds for. */
  def attempt[A](body: => A)(discard: A => Boolean): A = {
    routing()
    val tape     = Tape()
    val previous = current.get
    val held     = () => Option(tape)
    open.add(tape)
    current.set(tape)
    try {
      val value = Console.withOut(PrintStream(Gate(held, Console.out), true, UTF_8))(
        Console.withErr(PrintStream(Gate(held, Console.err), true, UTF_8))(body)
      )
      if (discard(value)) tape.close(): Unit
      value
    } finally {
      current.set(previous)
      replay(tape.close())
      open.remove(tape): Unit
    }
  }
}
