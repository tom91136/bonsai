package net.kurobako.bonsai.effects

import net.kurobako.bonsai.effects.Axioms.*

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

  private val inputStream = on("java.io.InputStream")

  test("only calls that reach the file system, the network or the host are input") {
    List(
      call("java.nio.file.Files.size"),
      call("java.io.File.length"),
      call("java.nio.file.Files.newInputStream"),
      call("java.nio.channels.FileChannel.open"),
      call("java.io.RandomAccessFile.length"),
      call("scala.io.Source.fromFile"),
      call("scala.io.StdIn.readLine"),
      call(
        "java.nio.file.attribute.BasicFileAttributeView.readAttributes",
        on("java.nio.file.attribute.AttributeView")
      ),
      call("java.lang.Process.exitValue"),
      call("java.lang.Runtime.availableProcessors"),
      call("java.awt.Toolkit.getScreenSize"),
      call("java.net.Socket.getInputStream")
    ).foreach(input => assertEquals(kinds(input), Set(Kind.Input), input.full))
    List("java.io.FileInputStream", "java.io.FileReader", "java.io.RandomAccessFile").foreach(owner =>
      assertEquals(kinds(Call(owner, "<init>", constructor = true)), Set(Kind.Input), owner)
    )
  }

  test("conversions and buffers in memory are not input") {
    List(
      call("java.nio.ByteBuffer.allocate"),
      call("java.nio.ByteBuffer.wrap"),
      call("java.nio.file.attribute.FileTime.fromMillis"),
      call("java.nio.file.attribute.FileTime.toMillis", on("java.nio.file.attribute.FileTime")),
      call("java.nio.file.attribute.BasicFileAttributes.size", on("java.nio.file.attribute.BasicFileAttributes")),
      call("java.nio.file.attribute.PosixFilePermissions.fromString"),
      call("java.nio.file.StandardOpenOption.values"),
      call("java.nio.charset.Charset.forName"),
      call("java.nio.ByteOrder.nativeOrder"),
      call("scala.io.Codec.fallbackSystemCodec"),
      call("java.io.IOException.getCause")
    ).foreach(pure => assertEquals(kinds(pure), Set.empty[Kind], pure.full))
    assertEquals(kinds(call("java.nio.ByteBuffer.putInt", on("java.nio.ByteBuffer"), java = true)), Set(Kind.Writes))
    assertEquals(kinds(call("java.nio.ByteBuffer.getInt", on("java.nio.ByteBuffer"), java = true)), Set(Kind.Reads))
  }

  test("reading a stream is input of the stream, which opening it decides") {
    List(
      call(
        "java.io.DataInputStream.readInt",
        on("java.io.FilterInputStream", "java.io.InputStream", "java.io.DataInput")
      ),
      call("java.io.ByteArrayInputStream.read", inputStream),
      call("java.io.FileInputStream.read", inputStream),
      call("java.io.BufferedReader.readLine", on("java.io.Reader")),
      call("java.io.InputStream.skip", inputStream),
      call("java.io.InputStream.readAllBytes", inputStream),
      call("java.nio.channels.FileChannel.read", on("java.nio.channels.ReadableByteChannel"))
    ).foreach { read =>
      val effects = Axioms.effects(read)
      assertEquals(effects.map(_.kind), Set(Kind.Input), read.full)
      assert(effects.forall(_.onThis), read.full)
    }
  }

  test("a thread or a component is read in the process, not from outside it") {
    assertEquals(kinds(call("java.lang.Thread.currentThread")), Set(Kind.Reads))
    assertEquals(kinds(call("java.lang.Thread.isInterrupted", on("java.lang.Thread"), java = true)), Set(Kind.Reads))
    assertEquals(kinds(call("javax.swing.JPanel.getWidth", on("javax.swing.JPanel"), java = true)), Set(Kind.Reads))
    assertEquals(kinds(call("java.awt.Component.isVisible", on("java.awt.Component"), java = true)), Set(Kind.Reads))
    assertEquals(kinds(call("java.lang.Thread.sleep")), Set(Kind.Output))
  }

  test("a probe asks the file system or the network and gets the same answer until something outside changes") {
    List("java.nio.file.Files.size", "java.nio.file.Files.exists", "java.nio.file.Files.isRegularFile").foreach(probe =>
      assert(Axioms.probes(call(probe)), probe)
    )
    assert(Axioms.probes(call("java.io.File.exists", on("java.io.File"), java = true)))
    assert(Axioms.probes(call("java.nio.file.Path.toRealPath", on("java.nio.file.Path"), java = true)))
    assert(Axioms.probes(call("java.net.InetAddress.getByName")))
    List(
      call("java.lang.System.getenv"),
      call("java.lang.System.currentTimeMillis"),
      call("java.nio.file.Files.write"),
      call("java.nio.file.Files.delete"),
      call("java.io.DataInputStream.readInt", on("java.io.InputStream")),
      call("scala.io.StdIn.readLine"),
      call("java.nio.ByteBuffer.allocate"),
      call("java.nio.file.Path.resolve")
    ).foreach(other => assert(!Axioms.probes(other), other.full))
    assert(!Axioms.probes(Call("java.io.FileInputStream", "<init>", constructor = true)))
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

  test("a Java class in a scala package is not covered by the table of the Scala library") {
    val node = on("scala.collection.concurrent.MainNode")
    assertEquals(kinds(call("scala.collection.concurrent.MainNode.CAS_PREV", node, java = true)), Set(Kind.Reads))
    assertEquals(
      kinds(call("scala.collection.concurrent.TrieMap.size", on("scala.collection.concurrent.TrieMap"))),
      Set.empty
    )
    assertEquals(kinds(call("java.lang.String.length", on("java.lang.String"), java = true)), Set.empty)
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
