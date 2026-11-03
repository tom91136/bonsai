package net.kurobako.bonsai.plugin

import dotty.tools.dotc.core.Contexts.Context
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.ReentrantLock

/** A value the first phase to ask for it computes. */
private class Deferred[A](compute: Context ?=> A) {
  private val lock     = ReentrantLock()
  private val computed = AtomicReference[Option[A]](None)

  def value(using Context): A = {
    lock.lock()
    try computed.get.getOrElse {
        val result = compute
        computed.set(Some(result))
        result
      }
    finally lock.unlock()
  }
}
