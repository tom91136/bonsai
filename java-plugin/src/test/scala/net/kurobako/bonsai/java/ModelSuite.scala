package net.kurobako.bonsai.java

import java.nio.file.Files
import net.kurobako.bonsai.collections.*
import net.kurobako.bonsai.testing.*

class ModelSuite extends munit.FunSuite {

  private val count = Model.Parameter("count", "int", "int", Vector("a", "b"), primitive = true)

  private val guards = Model.Guard.Connective(
    Model.Junction.Or,
    Model.Guard.Connective(
      Model.Junction.And,
      Model.Guard.Holds("a", Model.Probe.Present),
      Model.Guard.Holds("b", Model.Probe.Flag)
    ),
    Model.Guard.Not(
      Model.Guard.Connective(
        Model.Junction.Or,
        Model.Guard.Is("c", "LOAD"),
        Model.Guard.Connective(
          Model.Junction.And,
          Model.Guard.Compare("d", ">=", BigDecimal("-1.5")),
          Model.Guard.Known(false)
        )
      )
    )
  )

  private val costTree = {
    import Model.Cost.*
    Vector(
      Node(
        6,
        Step.Loop(
          "$1.items",
          callback = false,
          absorbs = 0,
          Vector(
            Node(
              7,
              Step.Loop("this.items", callback = true, absorbs = 1, Vector(Node(8, Step.Op(Kind.Concat(2), Unknown))))
            ),
            Node(9, Step.Call("demo.Outer.stop()V", Static, Vector("$0", Unknown), Sink.Dropped))
          )
        )
      ),
      Node(10, Step.Op(Kind.Sort, "$0", Sink.Stored)),
      Node(11, Step.Op(Kind.Shift, "$0", Sink(2, returned = true, stored = true))),
      Node(
        12,
        Step.Exit(Vector(
          Node(13, Step.Op(Kind.Search, "$0", Sink(1, returned = false, stored = false))),
          Node(14, Step.Call("demo.Outer.stop()V", Static, Vector.empty))
        ))
      )
    )
  }

  private val full = Model.Model(
    nodes = Vector(
      Model.Node("demo.Outer", "Outer.java", 3, exported = true, None, Set.empty),
      Model.Node(
        "demo.Outer.Inner",
        "Outer.java",
        9,
        exported = false,
        Some("demo.Outer"),
        Set("empty-method", "pointless-boolean"),
        entry = true,
        generated = true,
        constructible = true
      )
    ),
    methods = Vector(
      Model.Method(
        "demo.Outer",
        "run",
        "demo.Outer.run()I",
        exported = true,
        Set(0, 2),
        Vector(count),
        "Outer.java:5",
        "int",
        true,
        Set("x")
      )
    ),
    fields = Vector(Model.Field("demo.Outer", count)),
    sites =
      Vector(Model.Site("demo.Outer.run", "demo.Outer.stop", Vector("unknown", "a,b"), "Outer.java:6", true, true)),
    edges = Vector(Edge("demo.Outer", "demo.Outer.Inner")),
    fragments = Vector(
      Model.Fragment("demo.Outer", "Outer.java", 1, 4, 10, 80, 60, 7L, -9L, Vector("a,b", "c\td")),
      Model.Fragment(
        "demo.Outer",
        "Outer.java",
        6,
        6,
        90,
        120,
        14,
        3L,
        4L,
        Vector("1"),
        member = "run",
        broad = -5L,
        variables = Vector(Clones.Variable('R', "x", "x", 0, ""), Clones.Variable('L', "1", "1", -1, "")),
        roles = Model.Roles.Entire | Model.Roles.Helper,
        reach = "demo.Outer"
      )
    ),
    products = Vector(Model.Product("demo.Outer", "Outer", Vector(count), Set("demo.Base"))),
    sums = Vector(Model.Sum("demo.Shape", "Shape", Vector(Model.Variant("Circle")))),
    lints = Vector(Model.Lint("empty-method", "demo.Outer", "demo.Outer.run", "Outer.java", 5, "a\tb\nc")),
    reflections = Vector(Model.Reflection("demo.Outer.run", "demo.Hidden", "Outer.java:7")),
    constructions = Vector(
      Model.Construction("demo.Outer", "open", Vector.empty, Model.Guard.Known(true)),
      Model.Construction("demo.Outer.Inner", "factory", Vector("Inner.of", "Inner.parse"), guards)
    ),
    preconditions = Vector(Model.Precondition("demo.Outer.run", guards)),
    reassignables = Vector(
      Model.Reassignable("demo.Outer", "count", exported = true, "Outer.java", 4, Set("x")),
      Model.Reassignable("demo.Outer", "bare", exported = false, "Outer.java", 5, initialised = false)
    ),
    writes = Vector(
      Model.Write("demo.Outer", "count"),
      Model.Write("demo.Outer.Inner", "total"),
      Model.Write("demo.Outer", "bare", constructor = true)
    ),
    costs = Vector(
      Model.Cost(
        "demo.Outer",
        "demo.Outer.run",
        "demo.Outer.run([ILjava/util/List;)I",
        5,
        Set("x"),
        Vector("rows", "keys"),
        costTree
      ),
      Model.Cost("demo.Outer", "demo.Outer.stop", "demo.Outer.stop()V", 9, Set.empty, Vector.empty, Vector.empty)
    ),
    instances = Vector(
      Model.Instance("demo.Test.run", "demo.Outer", "demo.Outer.<init>(I)V", Vector("true", "text:a,b"), "Test.java:6")
    ),
    stores = Vector(
      Model.Store("demo.Outer", "demo.Outer.<init>(I)V", "count", Model.Stored.Parameter(0)),
      Model.Store("demo.Outer", "demo.Outer.<init>(I)V", "owned", Model.Stored.Constant("text:a,b"))
    )
  )

  private def read(lines: String*) =
    Scratch.using("bonsai-model") { directory =>
      val file = directory.resolve("model.tsv")
      Files.writeString(file, lines.mkString("", "\n", "\n"))
      Model.read(file)
    }

  test("a model written to a file is read back whole") {
    Scratch.using("bonsai-model") { directory =>
      val file = directory.resolve("model.tsv")
      Model.write(full, file)
      assertEquals(Model.read(file), Right(full))
    }
  }

  test("text the encoding cannot hold is written and read back whole") {
    val texts =
      Vector("\ud800", "a\udc00b", "\ud83d\ude00", "\ud83d", "x\r\ny", "\\ud800", "\u2028\u0085", ",\\c", "a|b\\p")
    val variables = texts.map(text => Clones.Variable('R', text, text, 0, text))
    val model     = Model.Model(
      nodes = Vector.empty,
      methods = Vector.empty,
      fields = Vector.empty,
      sites = Vector(Model.Site("demo.A.run", "demo.A.stop", texts, "A.java:1")),
      fragments = Vector(
        Model.Fragment("demo.A", "A.java", 1, 2, 3, 4, 5, 6L, 7L, texts, variables = variables)
      ),
      lints = Vector(Model.Lint("empty-method", "demo.A", "demo.A.run", "A.java", 1, texts.mkString("|"))),
      reflections = texts.map(text => Model.Reflection("demo.A.run", text, "A.java:1"))
    )
    Scratch.using("bonsai-model") { directory =>
      val file = directory.resolve("model.tsv")
      Model.write(model, file)
      assertEquals(Model.read(file), Right(model))
    }
  }

  test("every kind of record that is too short is reported by what it holds") {
    val short = Vector(
      "node"         -> "a node holds",
      "method"       -> "a method holds",
      "field"        -> "a field holds",
      "site"         -> "a call holds",
      "edge"         -> "a reference holds",
      "clone"        -> "a fragment holds",
      "product"      -> "a product holds",
      "sum"          -> "a sum holds",
      "lint"         -> "a lint holds",
      "reflect"      -> "a reflective lookup holds",
      "precondition" -> "a precondition holds",
      "reassignable" -> "a reassignable field holds",
      "write"        -> "a write holds",
      "cost"         -> "a cost holds",
      "instance"     -> "a construction holds",
      "store"        -> "a store holds"
    )
    short.foreach { (tag, message) =>
      val failure = read(s"$tag\tonly-one-field").swap.getOrElse(fail(s"a short $tag record was read"))
      assert(failure.contains(message), failure)
    }
  }

  test("every malformed record is reported, not only the first") {
    val failure =
      read("node\tshort", "edge\tshort", "edge\tfrom\tto").swap.getOrElse(fail("malformed records were read"))
    assertEquals(failure.linesIterator.size, 2, failure)
  }

  test("a malformed number in a record is reported") {
    val failure = read("node\tdemo.A\tA.java\tline\ttrue\t\t\tfalse\tfalse")
      .swap
      .getOrElse(fail("a malformed number was read"))
    assert(failure.contains("a node holds"), failure)
  }

  test("a malformed boolean in a record is reported") {
    val failure = read("node\tdemo.A\tA.java\t1\tyes\t\t\tfalse\tfalse")
      .swap
      .getOrElse(fail("a malformed boolean was read"))
    assert(failure.contains("a node holds"), failure)
  }

  test("a model that cannot be read from the file says so") {
    Scratch.using("bonsai-model") { directory =>
      val missing = directory.resolve("absent.tsv")
      assert(Model.read(missing).left.exists(_.contains("could not be read")))
    }
  }

  test("a guard is read back from the text it is written as") {
    assertEquals(Model.Guard.parse(guards.render), Some(guards))
    assertEquals(Model.Guard.parse(Model.Guard.Unknown.render), Some(Model.Guard.Unknown))
  }

  test("text that is not a guard is not read as one") {
    assertEquals(Model.Guard.parse("(present"), None)
    assertEquals(Model.Guard.parse("(and T)"), None)
    assertEquals(Model.Guard.parse("(compare a < b)"), None)
    assertEquals(Model.Guard.parse("T T"), None)
    assertEquals(Model.Guard.parse(""), None)
  }

  test("a tree of costs is read back from the text it is written as") {
    val text = Model.Cost.render(costTree)
    assertEquals(
      text,
      "{loop 6 $1.items {each:1 7 this.items {op 8 concat:2 ?}} {call 9 demo.Outer.stop()V - $0 ? @-}} " +
        "{op 10 sort $0 @0s} {op 11 shift $0 @2rs} {exit 12 {op 13 search $0 @1} {call 14 demo.Outer.stop()V -}}"
    )
    assertEquals(Model.Cost.parse(text), Some(costTree))
    assertEquals(Model.Cost.parse(""), Some(Vector.empty))
  }

  test("text that is not a tree of costs is not read as one") {
    Vector(
      "{loop 1}",
      "{loop x $0}",
      "{loop:x 1 $0}",
      "{loop 1 $0",
      "{exit}",
      "{exit x}",
      "{exit 1",
      "{op 1 weave $0}",
      "{op 1 sort}",
      "{op 1 sort $0 @x}",
      "{op 1 sort $0 @- @-}",
      "{op 1 sort $0 -}",
      "{call 1 key - $0 @0 @0}",
      "{call 1 key - @0q}",
      "{call 1 key}",
      "loop 1 $0",
      "{} {}"
    )
      .foreach(text => assertEquals(Model.Cost.parse(text), None, text))
  }

  test("a cost whose tree is malformed is reported rather than read as free") {
    val failure = read("cost\towner\tname\tkey\t1\t\t\t{loop 1").swap.getOrElse(fail("a malformed tree was read"))
    assert(failure.contains("key"), failure)
  }
}
