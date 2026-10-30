package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.requiredClass
import scala.collection.mutable

/** Reports blocking work handed to a shared pool sized for computation, waits with no limit, threads nothing stops, and
  * an interruptible wait retried by hand where an uninterruptible one exists.
  */
private class Concurrency(checks: Checks)(using Context) extends tpd.TreeTraverser {
  private val Thread        = requiredClass("java.lang.Thread")
  private val Future        = requiredClass("java.util.concurrent.Future")
  private val Completable   = requiredClass("java.util.concurrent.CompletableFuture")
  private val Interrupted   = requiredClass("java.lang.InterruptedException")
  private val InputStream   = requiredClass("java.io.InputStream")
  private val Reader        = requiredClass("java.io.Reader")
  private val Process       = requiredClass("java.lang.Process")
  private val Latch         = requiredClass("java.util.concurrent.CountDownLatch")
  private val Barrier       = requiredClass("java.util.concurrent.CyclicBarrier")
  private val Semaphore     = requiredClass("java.util.concurrent.Semaphore")
  private val BlockingQueue = requiredClass("java.util.concurrent.BlockingQueue")
  private val Executor      = requiredClass("java.util.concurrent.Executor")
  private val Channel       = requiredClass("java.nio.channels.Channel")
  private val ServerSocket  = requiredClass("java.net.ServerSocket")
  private val Socket        = requiredClass("java.net.Socket")
  private val Executing     = requiredClass("scala.concurrent.ExecutionContext")
  private val Service       = requiredClass("java.util.concurrent.ExecutorService")

  private val Blocking = Set(
    "java.lang.Thread.sleep",
    "java.lang.Object.wait",
    "scala.concurrent.Await.result",
    "scala.concurrent.Await.ready",
    "scala.io.StdIn.readLine"
  )
  private val Pooled =
    Set("java.util.concurrent.CompletableFuture.supplyAsync", "java.util.concurrent.CompletableFuture.runAsync")
  private val Globals =
    Set("scala.concurrent.ExecutionContext.global", "scala.concurrent.ExecutionContext.Implicits.global")
  private val Stops = Set("join", "interrupt")

  // What the enclosing methods finish before waiting on it, so the wait is short: futures checked for completion, run
  // on this thread or given a timeout, and processes killed. A method named for waiting leaves the limit to its callers, and is
  // recorded as finishing everything.
  private val done    = mutable.Stack(Set.empty[Symbol])
  private val waiting = mutable.Stack(false)
  // A close that interrupts its threads, or closes the channel or socket they block on, joins them as the barrier of its
  // shutdown, and an executor's execute starts the threads it runs tasks on, which it owns.
  private val stopping  = mutable.Stack(Int.MaxValue)
  private val executing = mutable.Stack(false)
  // The waits of a losing branch of a once-guard, by the start of their call.
  private val onced  = mutable.Set.empty[Int]
  private val Finish = Set("isDone", "run", "destroyForcibly", "orTimeout", "completeOnTimeout")
  private val Waits  = Set("await", "join")

  override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
    case definition: tpd.DefDef if definition.symbol.is(Flags.Synthetic) && !definition.symbol.isAnonymousFunction =>
      ()
    case definition: tpd.DefDef =>
      orphans(definition)
      join(definition)
      val finished = definition.rhs.filterSubTrees {
        case tpd.Select(_, name) => Finish(name.toString)
        case _                   => false
      }.collect { case tpd.Select(target, _) => target.symbol }
      done.push(done.top ++ finished ++ invoked(definition.rhs))
      waiting.push(waiting.top || Waits.exists(word => named(definition.name.toString, word)))
      stopping.push(stopping.top.min(stoppedFrom(definition)))
      executing.push(executing.top || executor(definition.symbol))
      try traverseChildren(tree)
      finally {
        done.pop()
        waiting.pop()
        stopping.pop()
        executing.pop(): Unit
      }
    case tpd.If(condition, thenp, elsep) =>
      if (Trees.selected(condition).exists(_.name.toString == "compareAndSet")) onced ++= guarded(thenp, elsep)
      traverseChildren(tree)
    case application: tpd.Apply =>
      pooled(application)
      unbounded(application)
      started(application)
      traverseChildren(tree)
    case _ => traverseChildren(tree)
  }

  // The start of the first call that stops the threads a close joins, from there on, when it closes a channel or
  // socket they block on or interrupts them.
  private def stoppedFrom(definition: tpd.DefDef): Int =
    if (definition.name.toString != "close") Int.MaxValue
    else if (
      definition.rhs.existsSubTree {
        case tpd.Select(thread, name) => name.toString == "interrupt" && thread.tpe.widen.derivesFrom(Thread)
        case _                        => false
      }
    ) 0
    else
      definition.rhs.filterSubTrees {
        case application: tpd.Apply => Trees.selected(application).exists(select =>
            select.name.toString == "close" && application.args.isEmpty && {
              val closed = select.qualifier.tpe.widen
              closed.derivesFrom(Channel) || closed.derivesFrom(ServerSocket) || closed.derivesFrom(Socket)
            }
          )
        case _ => false
      }.map(_.span.start).minOption.getOrElse(Int.MaxValue)

  // The waits on a latch that the other branch of a once-guard counts down in a finally, which the losing caller
  // waits for as the winner runs the work.
  private def guarded(winner: tpd.Tree, loser: tpd.Tree): Set[Int] = {
    def countsDown(tree: tpd.Tree): Option[Symbol] = tree match {
      case tpd.Apply(tpd.Select(latch, name), Nil)
          if name.toString == "countDown" && latch.tpe.widen.derivesFrom(Latch) =>
        Some(latch.symbol)
      case _ => None
    }
    val counted = winner.filterSubTrees(_.isInstanceOf[tpd.Try]).flatMap {
      case tpd.Try(_, _, finalizer) => finalizer.filterSubTrees(countsDown(_).isDefined).flatMap(countsDown)
      case _                        => Nil
    }.toSet
    loser.filterSubTrees {
      case tpd.Apply(tpd.Select(latch, name), _) => name.toString == "await" && counted(latch.symbol)
      case _                                     => false
    }.map(_.span.start).toSet
  }

  /** The name of a call that blocks its thread on IO, another thread, or time. */
  private def blocking(application: tpd.Apply): Option[String] = {
    val symbol                     = application.fun.symbol
    val name                       = symbol.name.toString
    def member(owner: ClassSymbol) = symbol.owner.derivesFrom(owner)
    Option.when(
      (member(InputStream) || member(Reader)) &&
        (name.startsWith("read") || name == "transferTo" || name == "skip") ||
        member(Process) && name == "waitFor" || member(Thread) && name == "join" ||
        member(Future) && name == "get" || member(Completable) && name == "join" ||
        (member(Latch) || member(Barrier)) && name == "await" || member(Semaphore) && name.startsWith("acquire") ||
        member(BlockingQueue) &&
        (name == "take" || name == "put") || Blocking(Trees.qualified(symbol))
    )(name)
  }

  // Calls inside `blocking` let the global pool add a thread for their duration.
  private def blockingCalls(body: tpd.Tree): List[String] = {
    val found = mutable.ListBuffer.empty[String]
    BlockingCalls(found).traverse(body)
    found.toList
  }

  private class BlockingCalls(found: mutable.ListBuffer[String]) extends tpd.TreeTraverser {
    override def traverse(tree: tpd.Tree)(using Context): Unit = tree match {
      case application: tpd.Apply if Trees.qualified(application.fun.symbol) == "scala.concurrent.package.blocking" =>
        ()
      case application: tpd.Apply =>
        found ++= blocking(application)
        traverseChildren(tree)
      case _ => traverseChildren(tree)
    }
  }

  private def pooled(application: tpd.Apply): Unit = application match {
    case tpd.Apply(function, List(task)) if Pooled(Trees.qualified(function.symbol)) =>
      blockingCalls(task).headOption.foreach(call =>
        checks(
          Check.BlockingCommonPool,
          s"${function.symbol.name} without an executor runs $call on the common pool, which has a thread per core; " +
            "pass an executor for blocking work",
          application.srcPos
        )
      )
    case tpd.Apply(tpd.Apply(function, List(body)), List(context))
        if Trees.qualified(function.symbol) == "scala.concurrent.Future.apply" &&
          Globals(Trees.qualified(Trees.unwrap(context).symbol)) =>
      blockingCalls(body).headOption.foreach(call =>
        checks(
          Check.BlockingCommonPool,
          s"a Future on the global pool runs $call outside blocking { }, holding one of its threads per core; wrap " +
            "it in blocking or pass an executor for blocking work",
          application.srcPos
        )
      )
    case _ => ()
  }

  private def unbounded(application: tpd.Apply): Unit = {
    val symbol  = application.fun.symbol
    val name    = symbol.name.toString
    val untimed = application.args.isEmpty && !synchronous(application) &&
      !(name == "join" && application.span.start >= stopping.top) && !onced(application.span.start) &&
      !waiting.top && !Trees.selected(application.fun).exists(select => bounded(select.qualifier)) &&
      blocking(application).exists(Set("get", "join", "waitFor", "await", "wait"))
    val unlimited = Set("scala.concurrent.Await.result", "scala.concurrent.Await.ready")(Trees.qualified(symbol)) &&
      application.args.lift(1).exists(limit =>
        Trees.qualified(Trees.unwrap(limit).symbol) == "scala.concurrent.duration.Duration.Inf"
      )
    if (untimed || unlimited)
      checks(
        Check.UnboundedWait,
        s"$name waits with no limit, so a stuck task hangs its caller; pass a timeout",
        application.srcPos
      )
  }

  // A future finished before the wait, or given a timeout by the call it comes from.
  private def bounded(future: tpd.Tree): Boolean = done.top(future.symbol) ||
    (Trees.unwrap(future) match {
      case application: tpd.Apply => Set("orTimeout", "completeOnTimeout")(application.fun.symbol.name.toString)
      case _                      => false
    })

  // A name of words in camel case starting with `word`, as `awaitTermination` starts with `await`.
  private def named(name: String, word: String): Boolean =
    name.startsWith(word) && name.lift(word.length).forall(_.isUpper)

  // Joining a thread the method holds locally waits only for work the method itself runs, as a call would.
  private def synchronous(application: tpd.Apply): Boolean = Trees.selected(application.fun).exists { select =>
    val receiver = select.qualifier.symbol
    select.qualifier.tpe.widen.derivesFrom(Thread) && receiver.isTerm && !receiver.owner.isClass &&
    !receiver.is(Flags.Param)
  }

  private def constructed(tree: tpd.Tree): Boolean = Trees.unwrap(tree) match {
    case application: tpd.Apply => application.fun.symbol.isConstructor
    case _                      => false
  }

  private def started(application: tpd.Apply): Unit = application match {
    case tpd.Apply(tpd.Select(thread, name), Nil)
        if !executing.top && name.toString == "start" && thread.tpe.widen.derivesFrom(Thread) && constructed(thread) =>
      checks(
        Check.OrphanThread,
        "the thread started here is never joined or interrupted; keep it and stop it when its owner shuts down",
        application.srcPos
      )
    case _ => ()
  }

  private def executor(method: Symbol): Boolean = method.name.toString == "execute" &&
    method.allOverriddenSymbols.exists(overridden => overridden.owner == Executor || overridden.owner == Executing)

  /** Futures that `invokeAll` returns, which it returns only once they are all complete: the values holding its result,
    * and the parameters of the functions run over it.
    */
  private def invoked(body: tpd.Tree): Set[Symbol] = body.filterSubTrees {
    case _: tpd.ValDef | _: tpd.Apply => true
    case _                            => false
  }.foldLeft(Set.empty[Symbol]) {
    case (complete, value: tpd.ValDef) if completed(value.rhs, complete) => complete + value.symbol
    case (complete, application: tpd.Apply)
        if Trees.selected(application.fun).exists(select => completed(select.qualifier, complete)) =>
      complete ++ application.args.flatMap(Trees.lambda(_)).flatMap(_.termParamss.flatten.headOption.map(_.symbol))
    case (complete, _) => complete
  }

  // Conversions and collection operations keep the futures of an invokeAll result.
  private def completed(tree: tpd.Tree, complete: Set[Symbol]): Boolean = Trees.unwrap(tree) match {
    case application: tpd.Apply
        if application.fun.symbol.name.toString == "invokeAll" && application.fun.symbol.owner.derivesFrom(Service) =>
      true
    case tpd.Apply(function, List(argument)) if function.symbol.is(Flags.Implicit) => completed(argument, complete)
    case application: tpd.Apply                                                    =>
      Trees.selected(application.fun).exists(select => completed(select.qualifier, complete))
    case tpd.Select(qualifier, _) => completed(qualifier, complete)
    case reference: tpd.Ident     => complete(reference.symbol)
    case _                        => false
  }

  /** Local threads used only through members other than those that stop them. */
  private def orphans(definition: tpd.DefDef): Unit = if (!executing.top && !executor(definition.symbol))
    definition.rhs.filterSubTrees {
      case value: tpd.ValDef =>
        value.symbol.exists && !value.symbol.owner.isClass && value.symbol.info.derivesFrom(Thread) &&
        constructed(value.rhs)
      case _ => false
    }.foreach { value =>
      val references = definition.rhs.filterSubTrees {
        case reference: tpd.Ident => reference.symbol == value.symbol
        case _                    => false
      }
      val members = definition.rhs.filterSubTrees {
        case tpd.Select(reference: tpd.Ident, _) => reference.symbol == value.symbol
        case _                                   => false
      }.collect { case select: tpd.Select => select.name.toString }
      if (references.size == members.size && members.contains("start") && !members.exists(Stops))
        checks(
          Check.OrphanThread,
          s"${value.symbol.name} is started but never joined or interrupted; stop it when its owner shuts down",
          value.srcPos
        )
    }

  // CompletableFuture.join waits through interrupts, leaving the interrupt flag set.
  private def join(definition: tpd.DefDef): Unit = {
    val gets = definition.rhs.filterSubTrees {
      case tpd.Apply(tpd.Select(future, name), Nil) =>
        name.toString == "get" && future.tpe.widen.derivesFrom(Completable)
      case _ => false
    }
    val retried = definition.rhs.existsSubTree {
      case tpd.Typed(_, tpt) => tpt.tpe.derivesFrom(Interrupted)
      case _                 => false
    }
    val restored = definition.rhs.existsSubTree {
      case application: tpd.Apply => Trees.qualified(application.fun.symbol) == "java.lang.Thread.interrupt"
      case _                      => false
    }
    if (retried && restored)
      gets.foreach(get =>
        checks(
          Check.ReimplementedJoin,
          "get retried through interrupts, restoring the interrupt flag afterwards, reimplements " +
            "CompletableFuture.join; call join",
          get.srcPos
        )
      )
  }
}
