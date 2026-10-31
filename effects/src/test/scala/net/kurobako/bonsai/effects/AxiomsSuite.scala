package net.kurobako.bonsai.effects

import net.kurobako.bonsai.effects.Axioms.Call
import net.kurobako.bonsai.effects.Axioms.Operand

class AxiomsSuite extends munit.FunSuite {
  private def on(classes: String*): Option[Operand] = Some(Operand(classes.toSet))
  private def kinds(call: Call): Set[Kind]          = Axioms.effects(call).map(_.kind)
  private def call(full: String, receiver: Option[Operand] = None, java: Boolean = false): Call =
    Call(
      full.substring(0, full.lastIndexOf('.')),
      full.substring(full.lastIndexOf('.') + 1),
      receiver = receiver,
      java = java
    )

  private val prefixed = Set(
    "addOne",
    "addAll",
    "add",
    "remove",
    "clear",
    "clearAndShrink",
    "put",
    "putAll",
    "set",
    "setLength",
    "sortInPlace",
    "mapInPlace",
    "filterInPlace",
    "flatMapInPlace",
    "patchInPlace",
    "dropInPlace",
    "takeInPlace"
  )

  test("clocks and every method of a random generator are nondeterministic") {
    Axioms.Clocks.foreach(clock => assertEquals(kinds(call(clock)), Set(Kind.Random), clock))
    Axioms.Randoms.foreach { generator =>
      List("nextInt", "nextLong", "between", "shuffle", "ints", "longs", "doubles", "alphanumeric", "setSeed").foreach {
        name => assertEquals(kinds(call(s"$generator.$name", on(generator))), Set(Kind.Random), s"$generator.$name")
      }
    }
    assertEquals(kinds(call("scala.util.Random.nextInt")), Set(Kind.Random))
  }

  test("every mutator writes the receiver of a stateful class") {
    (Axioms.Mutators ++ prefixed).foreach { name =>
      val effects =
        Axioms.effects(call(s"scala.collection.mutable.Buffer.$name", on("scala.collection.mutable.Iterable")))
      assertEquals(effects.map(_.kind), Set(Kind.Writes), name)
      assert(effects.forall(_.onThis), name)
    }
  }

  test("atomics are written by the names either table listed") {
    val owner  = "java.util.concurrent.atomic.AtomicInteger"
    val writes = Axioms.Mutators ++ prefixed ++ Set(
      "set",
      "lazySet",
      "compareAndSet",
      "weakCompareAndSet",
      "weakCompareAndSetPlain",
      "compareAndExchange",
      "getAndSet",
      "getAndIncrement",
      "getAndDecrement",
      "getAndAdd",
      "getAndAccumulate",
      "getAndUpdate",
      "incrementAndGet",
      "decrementAndGet",
      "addAndGet",
      "accumulateAndGet",
      "updateAndGet",
      "increment",
      "decrement",
      "add",
      "update",
      "accumulate",
      "reset"
    )
    writes.foreach(name => assertEquals(kinds(call(s"$owner.$name")), Set(Kind.Writes), name))
    List("get", "getPlain", "intValue", "sum").foreach(name =>
      assertEquals(kinds(call(s"$owner.$name")), Set(Kind.Reads), name)
    )
  }

  test("output, input and hidden state") {
    assertEquals(kinds(call("scala.Predef.println")), Set(Kind.Output))
    assertEquals(kinds(call("scala.Predef.printf")), Set(Kind.Output))
    assertEquals(kinds(call("org.slf4j.Logger.info")), Set(Kind.Output))
    assertEquals(kinds(call("scala.Console.println")), Set(Kind.Output))
    assertEquals(kinds(call("java.io.PrintStream.println", on("java.io.OutputStream"))), Set(Kind.Output))
    assertEquals(kinds(call("java.lang.System.getenv")), Set(Kind.Input))
    assertEquals(kinds(call("java.lang.System.exit")), Set(Kind.Output))
    assertEquals(kinds(call("java.lang.System.lineSeparator")), Set.empty)
    assertEquals(kinds(call("java.lang.Thread.sleep")), Set(Kind.Output))
    assertEquals(kinds(call("java.lang.Thread.currentThread")), Set(Kind.Input))
    assertEquals(kinds(call("java.net.URI.create")), Set.empty)
    assertEquals(kinds(call("java.nio.file.Files.write")), Set(Kind.Output))
    assertEquals(kinds(call("java.nio.file.Files.readString")), Set(Kind.Input))
    assertEquals(kinds(call("java.nio.file.Path.toAbsolutePath")), Set(Kind.Input))
    assertEquals(kinds(call("java.nio.file.Path.resolve")), Set.empty)
    assertEquals(kinds(call("java.io.File.getName")), Set.empty)
    assertEquals(kinds(call("java.io.File.exists")), Set(Kind.Input))
    assertEquals(kinds(call("scala.io.Source.fromFile")), Set(Kind.Input))
    assertEquals(kinds(call("scala.Array.copy")), Set(Kind.Writes))
    assertEquals(kinds(call("scala.collection.ArrayOps.copyToArray")), Set(Kind.Writes))
  }

  test("partial methods of pure owners throw, and a companion's apply does not") {
    (Partials.Names - "<init>").foreach(name =>
      assertEquals(kinds(call(s"scala.Predef.$name")), Set(Kind.Throws), name)
    )
    assertEquals(kinds(call("scala.Predef.require")), Set(Kind.Throws))
    assertEquals(kinds(call("scala.Predef.checkNotNull")), Set(Kind.Throws))
    assertEquals(
      Axioms.effects(Call(
        "scala.collection.immutable.List",
        "apply",
        receiver = Some(Operand(_ => false, module = true))
      )),
      Set.empty
    )
    assertEquals(kinds(call("scala.collection.immutable.List.map")), Set.empty)
  }

  test("unknown code is unknown, a Java object may be read or written, a constructor has no effect of its own") {
    assertEquals(kinds(call("demo.Service.run")), Set(Kind.Unknown))
    assertEquals(kinds(call("demo.Handle.fill", on("demo.Handle"), java = true)), Set(Kind.Reads))
    assertEquals(kinds(call("demo.Handle.setName", on("demo.Handle"), java = true)), Set(Kind.Writes))
    assertEquals(kinds(call("demo.Handle.close", on("demo.Handle"), java = true)), Set(Kind.Writes))
    assertEquals(Axioms.effects(Call("scala.util.Random", "<init>", constructor = true)), Set.empty)
    assertEquals(Axioms.summary(Call("demo.Service", "<init>", constructor = true), Set(0)).invokes, Set.empty[Int])
    assertEquals(Axioms.summary(call("demo.Service.run"), Set(0)).invokes, Set(0))
    assertEquals(kinds(Call("demo.Suite", "check", returns = Axioms.Assertion)), Set(Kind.Throws))
  }
}
