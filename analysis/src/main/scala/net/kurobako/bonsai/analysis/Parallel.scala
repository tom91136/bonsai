package net.kurobako.bonsai.analysis

import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.ThreadFactory
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

/** Runs independent work on a fixed number of threads. The work must not touch the compiler's context, its symbols, or
  * state shared with other work, and must not wait on other work spawned here, so the results do not depend on which
  * thread ran what or when.
  */
sealed trait Parallel extends AutoCloseable {

  /** Starts `work`, and returns what joins it: the result, or whatever `work` threw. */
  def spawn[A](work: () => A): () => A

  /** `f` over `inputs`, in their order whichever finishes first. */
  def map[A, B](inputs: Vector[A])(f: A => B): Vector[B] = inputs.map(input => spawn(() => f(input))).map(_())
}

object Parallel {

  /** Runs each piece of work on the thread that joins it, when it does. */
  val sequential: Parallel = new Parallel {
    def spawn[A](work: () => A): () => A = {
      lazy val result = work()
      () => result
    }

    def close(): Unit = ()
  }

  /** A pool of `threads` workers, or `sequential` for one thread. Closing it stops the workers. */
  def apply(threads: Int): Parallel = if (threads <= 1) sequential else Pool(threads)

  private class Pool(threads: Int) extends Parallel {
    private val counter  = AtomicInteger()
    private val executor = ThreadPoolExecutor(
      threads,
      threads,
      0L,
      TimeUnit.MILLISECONDS,
      LinkedBlockingQueue[Runnable](),
      new ThreadFactory {
        def newThread(task: Runnable): Thread = {
          val thread = Thread(task, s"bonsai-worker-${counter.incrementAndGet()}")
          thread.setDaemon(true)
          thread
        }
      }
    )

    // Whoever joins has nothing else to do, and the work is bounded by the files it reads.
    @SuppressWarnings(Array("bonsai:unbounded-wait"))
    def spawn[A](work: () => A): () => A = {
      val task: Future[A] = executor.submit[A](() => work())
      () =>
        try task.get()
        catch { case failed: ExecutionException => throw failed.getCause }
    }

    def close(): Unit = executor.shutdownNow(): Unit
  }
}
