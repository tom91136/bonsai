package net.kurobako.bonsai.analysis

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import munit.FunSuite

class ParallelSuite extends FunSuite {
  private def using[A](threads: Int)(body: Parallel => A): A = {
    val parallel = Parallel(threads)
    try body(parallel)
    finally parallel.close()
  }

  test("a single thread runs the work on the calling thread, when the result is joined") {
    using(1) { parallel =>
      val caller = Thread.currentThread
      var ran    = false
      val joined = parallel.spawn { () =>
        ran = true
        Thread.currentThread
      }
      assert(!ran)
      assertEquals(joined(), caller)
      assert(ran)
    }
  }

  test("results keep the order of the inputs, whichever finishes first") {
    using(4) { parallel =>
      val released = CountDownLatch(1)
      val results  = parallel.map(Vector(1, 2, 3, 4)) { value =>
        if (value == 1) assert(released.await(10, TimeUnit.SECONDS), "the last task never ran beside the first")
        else if (value == 4) released.countDown()
        value * 10
      }
      assertEquals(results, Vector(10, 20, 30, 40))
    }
  }

  test("work spawned before a join runs beside the caller") {
    using(2) { parallel =>
      val started = CountDownLatch(1)
      val joined  = parallel.spawn { () =>
        started.countDown()
        Thread.currentThread.getName
      }
      assert(started.await(10, TimeUnit.SECONDS))
      assert(joined().startsWith("bonsai-worker"))
    }
  }

  private def thrown(work: () => Int): Throwable =
    try
      fail(s"returned ${work()}")
    catch { case error: Throwable => error }

  test("a failure surfaces when its result is joined, with the error unwrapped") {
    for (threads <- Seq(1, 4)) using(threads) { parallel =>
      assertEquals(thrown(parallel.spawn(() => throw IllegalArgumentException("bad"))).getMessage, "bad")
      assert(thrown(parallel.spawn(() => throw StackOverflowError())).isInstanceOf[StackOverflowError])
    }
  }

  test("closing stops the workers") {
    val workers = using(3)(_.map(Vector(1, 2, 3))(_ => Thread.currentThread))
    workers.foreach(_.join(10000))
    assert(workers.forall(!_.isAlive))
  }
}
