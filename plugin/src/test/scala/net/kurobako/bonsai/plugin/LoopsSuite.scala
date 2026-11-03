package net.kurobako.bonsai.plugin

import Warnings.*
import munit.FunSuite

class LoopsSuite extends FunSuite {
  private def warnings(source: String, options: String*): List[String] = Warnings.of(source, options*)

  test("work that a recursion passes unchanged to every call is reported where the calls repeat it") {
    val found = warnings(
      """object Lints {
        |  @scala.annotation.tailrec
        |  def joined(names: Vector[String], rounds: Int, acc: Int): Int =
        |    if (rounds == 0) acc
        |    else {
        |      val children = names.groupBy(_.toUpperCase)
        |      joined(names, rounds - 1, acc + children.size)
        |    }
        |  def distinct(values: List[Int], index: Int, depth: Int): Boolean =
        |    if (depth == 0) false else values.toSet(index) || distinct(values, index, depth - 1)
        |  class Planner(names: Vector[String]) {
        |    def levels(depth: Int): Int =
        |      if (depth == 0) 0 else names.map(_.length).toSet.size + levels(depth - 1)
        |  }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "invariant-in-loop"),
      List(
        "names.groupBy(_.toUpperCase) does not depend on what the recursion of joined changes",
        "names.map(_.length) does not depend on what the recursion of levels changes",
        "values.toSet does not depend on what the recursion of distinct changes"
      ).sorted
    )
  }

  test("a literal set a recursion builds on every call is a table rebuilt in a loop") {
    val found = warnings(
      """object Lints {
        |  def retained(name: String, depth: Int): Boolean =
        |    Set("count", "head", "last")(name) || depth > 0 && retained(name, depth - 1)
        |  def once(name: String): Boolean = Set("size", "length", "isEmpty")(name)
        |  def base(name: String, depth: Int): Boolean =
        |    if (depth == 0) Set("find", "exists", "forall")(name) else base(name, depth - 1)
        |}""".stripMargin
    )
    assertEquals(tagged(found, "inline-set-membership").size, 1, found.mkString("\n"))
    assert(tagged(found, "inline-set-membership").head.contains("rebuilt and hashed on every iteration"))
  }

  test("work that depends on what a call changes, or runs in a base case, or builds a record, is not reported") {
    val found = warnings(
      """case class Point(x: Int, y: Int)
        |sealed trait Tree { def tags: Vector[String] }
        |case class Branch(children: List[Tree], tags: Vector[String]) extends Tree
        |case class Leaf(tags: Vector[String]) extends Tree
        |object Lints {
        |  def shrinking(items: List[Int]): Int = if (items.isEmpty) 0 else items.toSet.size + shrinking(items.tail)
        |  def mixed(names: Vector[String], flag: Boolean, depth: Int): Int =
        |    if (depth == 0) 0
        |    else if (flag) names.toSet.size + mixed(names.tail, flag, depth - 1)
        |    else mixed(names, flag, depth - 1)
        |  def base(names: Vector[String], depth: Int): Map[String, Vector[String]] =
        |    if (depth == 0) names.groupBy(_.toUpperCase) else base(names, depth - 1)
        |  def record(base: Int, depth: Int): Int = {
        |    val point = Point(base, 1)
        |    if (depth == 0) point.x else record(base, depth - 1)
        |  }
        |  def inner(tree: Tree): Int = tree match {
        |    case Branch(children, tags) => tags.toSet.size + children.map(inner).sum
        |    case Leaf(tags)             => tags.toSet.size
        |  }
        |  def lazily(names: Vector[String], depth: Int): Int = {
        |    lazy val unique = names.toSet
        |    if (depth == 0) unique.size else lazily(names, depth - 1) + unique.size
        |  }
        |  def once(names: Vector[String]): Int = names.groupBy(_.toUpperCase).size
        |}""".stripMargin
    )
    assertEquals(tagged(found, "invariant-in-loop"), Nil, found.mkString("\n"))
  }

  test("a recursion that calls itself on another receiver is not a loop over its fields") {
    val found = warnings(
      """class Node(val children: List[Node], val tags: Vector[String]) {
        |  def depth(levels: Int): Int =
        |    if (levels == 0) 0 else tags.toSet.size + children.map(_.depth(levels - 1)).sum
        |}""".stripMargin
    )
    assertEquals(tagged(found, "invariant-in-loop"), Nil, found.mkString("\n"))
  }

  test("scans of a collection the loop does not change, relating its elements to the loop variable, are reported") {
    val found = warnings(
      """case class Item(id: Int, owner: String)
        |object Lints {
        |  def referrers(edges: Map[String, Set[String]], nodes: Vector[String]): Vector[Vector[String]] =
        |    nodes.map(node => edges.collect { case (from, targets) if from != node && targets(node) => from }.toVector)
        |  def owned(items: Vector[Item], keys: Vector[String]): Vector[Vector[Item]] =
        |    keys.map(key => items.filter(_.owner == key))
        |  def present(items: Vector[Item], keys: Vector[String]): Int =
        |    keys.count(key => items.exists(item => key == item.owner))
        |  def counted(items: Vector[Item], ids: Vector[Int]): Vector[Int] =
        |    ids.map(id => items.count(_.id == id))
        |  def located(items: List[Item], ids: Vector[Int]): Vector[Option[Item]] =
        |    ids.map(id => items.find(item => item.id == id && item.owner.nonEmpty))
        |  def loop(items: Vector[Item], ids: Vector[Int]): Int = {
        |    var total = 0
        |    var index = 0
        |    while (index < ids.size) { total += items.count(_.id == ids(index)); index += 1 }
        |    total
        |  }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "keyed-lookup-in-loop"),
      List(
        "collect over edges relates its elements to the loop variable and scans them on every iteration",
        "filter over items relates its elements to the loop variable and scans them on every iteration",
        "exists over items relates its elements to the loop variable and scans them on every iteration",
        "count over items relates its elements to the loop variable and scans them on every iteration",
        "find over items relates its elements to the loop variable and scans them on every iteration",
        "count over items relates its elements to the loop variable and scans them on every iteration"
      ).sorted
    )
  }

  test("scans that do not relate the elements to the loop variable by equality or membership are not reported") {
    val found = warnings(
      """case class Item(id: Int, owner: String, alias: String, children: Vector[Item])
        |object Lints {
        |  def prefixed(items: Vector[Item], keys: Vector[String]): Vector[Vector[Item]] =
        |    keys.map(key => items.filter(_.owner.startsWith(key)))
        |  def ordered(items: Vector[Item], ids: Vector[Int]): Vector[Boolean] = ids.map(id => items.exists(_.id > id))
        |  def either(items: Vector[Item], keys: Vector[String]): Vector[Boolean] =
        |    keys.map(key => items.exists(item => item.owner == key || item.alias == key))
        |  def constant(items: Vector[Item], keys: Vector[String]): Vector[Boolean] =
        |    keys.map(_ => items.exists(_.owner == "root"))
        |  def derived(items: Vector[Item]): Vector[Option[Item]] = items.map(item => item.children.find(_.id == item.id))
        |  def listed(keys: Vector[String]): Vector[Boolean] = keys.map(key => List("a", "b").exists(_ == key))
        |  def sets(items: Set[Int], ids: Vector[Int]): Vector[Boolean] = ids.map(items.contains)
        |  def ranges(ids: Vector[Int]): Vector[Boolean] = ids.map(id => (0 until 10).exists(_ == id))
        |  def once(items: Vector[Item], key: String): Vector[Item] = items.filter(_.owner == key)
        |  def contained(items: Vector[Item], groups: Vector[Set[Int]]): Vector[Vector[Item]] =
        |    groups.map(group => items.filter(item => group(item.id)))
        |}""".stripMargin
    )
    assertEquals(tagged(found, "keyed-lookup-in-loop"), Nil, found.mkString("\n"))
  }

  test("a scan of the collection a loop iterates makes the loop quadratic") {
    val found = warnings(
      """case class Item(id: Int, parent: Int)
        |object Lints {
        |  def duplicates(values: Vector[Int]): Vector[Int] = values.map(value => values.count(_ == value))
        |  def roots(items: Vector[Item]): Vector[Item] = items.filter(item => !items.exists(_.id == item.parent))
        |  def firsts(names: List[String]): Vector[Int] = {
        |    val result = Vector.newBuilder[Int]
        |    names.foreach(name => result += names.indexWhere(_ == name))
        |    result.result()
        |  }
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "linear-in-loop").filter(_.contains("iterates")),
      List(
        "count over values scans the collection the loop iterates, which makes it quadratic",
        "exists over items scans the collection the loop iterates, which makes it quadratic",
        "indexWhere over names scans the collection the loop iterates, which makes it quadratic"
      ).sorted
    )
  }

  test("scans of another collection, or of one built for the element, are not scans of the iterated one") {
    val found = warnings(
      """case class Item(id: Int, children: Vector[Item])
        |object Lints {
        |  def sized(values: Vector[Int]): Vector[Int] = values.map(value => values.size + value)
        |  def other(values: Vector[Int], more: Vector[Int]): Vector[Int] = values.map(value => more.count(_ == value))
        |  def children(items: Vector[Item]): Vector[Int] = items.map(item => item.children.count(_.id == item.id))
        |  def sets(values: Set[Int]): Vector[Boolean] = values.toVector.map(value => values.contains(value))
        |  def once(values: Vector[Int]): Int = values.count(_ == 1)
        |}""".stripMargin
    )
    assertEquals(tagged(found, "linear-in-loop").filter(_.contains("iterates")), Nil, found.mkString("\n"))
  }

  test("a call that walks a recursive structure not rooted at the element is reported per element") {
    val found = warnings(
      """sealed trait Tree
        |case class Node(children: List[Tree]) extends Tree
        |case class Leaf(name: String) extends Tree
        |object Lints {
        |  def mentions(tree: Tree, name: String): Boolean = tree match {
        |    case Leaf(n)      => n == name
        |    case Node(parts)  => parts.exists(mentions(_, name))
        |  }
        |  def size(tree: Tree): Int = tree match {
        |    case Leaf(_)      => 1
        |    case Node(parts)  => parts.map(size).sum
        |  }
        |  def unused(root: Tree, names: Vector[String]): Vector[String] = names.filterNot(name => mentions(root, name))
        |  def rooted(roots: Vector[Tree], name: String): Vector[Boolean] = roots.map(mentions(_, name))
        |  def invariant(root: Tree, names: Vector[String]): Vector[Int] = names.map(_ => size(root))
        |  def single(root: Tree, name: String): Boolean = mentions(root, name)
        |  def evaluate(tree: Tree, env: Vector[String]): Boolean = tree match {
        |    case Leaf(n)     => env.contains(n)
        |    case Node(parts) => parts.exists(evaluate(_, env))
        |  }
        |  def evaluated(root: Tree, envs: Vector[Vector[String]]): Vector[Boolean] = envs.map(env => evaluate(root, env))
        |  def same(left: Tree, right: Tree): Boolean = (left, right) match {
        |    case (Leaf(a), Leaf(b))         => a == b
        |    case (Node(as), Node(bs))       => as.size == bs.size && as.zip(bs).forall((a, b) => same(a, b))
        |    case _                          => false
        |  }
        |  def equal(root: Tree, others: Vector[Tree]): Vector[Boolean] = others.map(other => same(root, other))
        |  case class Scope(parent: Option[Scope])
        |  def seen(name: String)(using scope: Scope): Boolean = scope.parent.exists(parent => seen(name)(using parent))
        |  def scoped(names: Vector[String])(using scope: Scope): Vector[Boolean] = names.map(name => seen(name))
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "linear-in-loop").filter(_.contains("walks")),
      List("mentions walks root on every iteration")
    )
  }

  test("an aggregate over a window of a collection that grows by one element on each iteration is reported") {
    val found = warnings(
      """object Lints {
        |  def windows(items: Vector[Int], n: Int): Vector[Int] =
        |    (0 until n).flatMap(start => (start until n).map(end => items.slice(start, end + 1).sum)).toVector
        |  def prefixes(items: Vector[Int]): Vector[Int] = items.indices.map(index => items.take(index).sum).toVector
        |  def mapped(items: Vector[Int], n: Int): Vector[Int] =
        |    (0 until n).flatMap(start => (start until n).map(end => items.slice(start, end + 1).map(_ * 2).sum)).toVector
        |  def bound(items: Vector[Int], n: Int): Vector[Int] =
        |    (0 until n).flatMap(start => (start until n).map { end =>
        |      val window = items.slice(start, end + 1)
        |      window.sum + window.size
        |    }).toVector
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "linear-in-loop").filter(_.contains("window")),
      List(
        "slice recomputes the aggregate over a window that grows by one element on every iteration",
        "slice recomputes the aggregate over a window that grows by one element on every iteration",
        "slice recomputes the aggregate over a window that grows by one element on every iteration",
        "take recomputes the aggregate over a window that grows by one element on every iteration"
      )
    )
  }

  test("windows of a fixed size, windows that are not aggregated, and a window outside a loop are not reported") {
    val found = warnings(
      """object Lints {
        |  def fixed(items: Vector[Int], n: Int): Vector[Int] =
        |    (0 until n).map(start => items.slice(start, start + 3).sum).toVector
        |  def head(items: Vector[Int], n: Int): Vector[Int] = (0 until n).map(_ => items.take(3).sum).toVector
        |  def kept(items: Vector[Int], n: Int): Vector[Vector[Int]] =
        |    (0 until n).flatMap(start => (start until n).map(end => items.slice(start, end + 1))).toVector
        |  def counted(items: Vector[Int], n: Int): Vector[Int] =
        |    (0 until n).flatMap(start => (start until n).map(end => items.slice(start, end + 1).size)).toVector
        |  def once(items: Vector[Int], n: Int): Int = items.slice(1, n).sum
        |}""".stripMargin
    )
    assertEquals(tagged(found, "linear-in-loop").filter(_.contains("window")), Nil, found.mkString("\n"))
  }

  test("a scan inside a lambda that depends neither on its parameters nor on the loop is hoistable") {
    val found = warnings(
      """object Lints {
        |  def dropAssigned(refs: Vector[String], assigned: Vector[String], index: String): Vector[String] =
        |    refs.filterNot(ref => ref == index && assigned.exists(_.startsWith(index)))
        |  def scaled(values: Vector[Double], scale: Vector[Double]): Vector[Double] = values.map(_ / scale.sum)
        |  def joined(values: Vector[String], parts: Vector[String]): Vector[String] =
        |    values.map(_ + parts.mkString(","))
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "invariant-in-loop"),
      List(
        "assigned.exists(_.startsWith(index)) does not depend on the loop",
        "parts.mkString(\",\") does not depend on the loop",
        "scale.sum does not depend on the loop"
      ).sorted
    )
  }

  test("a scan inside a lambda that depends on its parameter, on the loop or on state, is not reported") {
    val found = warnings(
      """object Lints {
        |  def element(refs: Vector[String], assigned: Vector[String]): Vector[String] =
        |    refs.filterNot(ref => assigned.exists(_.startsWith(ref)))
        |  def counted(refs: Vector[String], assigned: Vector[String]): Int = {
        |    var seen = 0
        |    refs.foreach(ref => if (assigned.exists(_ => { seen += 1; seen > 2 })) seen += 1)
        |    seen
        |  }
        |  def built(refs: Vector[Vector[String]]): Vector[Boolean] = refs.map(ref => ref.exists(_.isEmpty))
        |  def sets(refs: Vector[String], assigned: Set[String]): Vector[Boolean] = refs.map(ref => assigned.contains(ref))
        |  def sizes(refs: Vector[String], assigned: Vector[String]): Vector[Int] = refs.map(_.length + assigned.size)
        |  def once(assigned: Vector[String], index: String): Boolean = assigned.exists(_.startsWith(index))
        |  def optional(refs: Option[String], assigned: Vector[String]): Vector[String] =
        |    for (ref <- refs.toVector if assigned.exists(_.isEmpty)) yield ref
        |}""".stripMargin
    )
    assertEquals(tagged(found, "invariant-in-loop"), Nil, found.mkString("\n"))
  }

  test("work derived from the elements of an immutable field alone is recomputed on every call") {
    val found = warnings(
      """class Namespaces(packages: Vector[Vector[String]], names: Set[String]) {
        |  def namespaced(name: String): Boolean = packages.exists(root => name.startsWith(root.mkString("", "/", "/")))
        |  def lowered(name: String): Boolean = names.exists(candidate => candidate.toLowerCase == name)
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "invariant-in-loop"),
      List(
        "candidate.toLowerCase depends only on the elements of names",
        "root.mkString(\"\", \"/\", \"/\") depends only on the elements of packages"
      ).sorted
    )
  }

  test("work that reads a parameter or a variable, or a collection that is not a field, is not derived from a field") {
    val found = warnings(
      """class Namespaces(packages: Vector[Vector[String]]) {
        |  var mutable: Vector[Vector[String]] = Vector.empty
        |  def separated(name: String, separator: String): Boolean =
        |    packages.exists(root => name.startsWith(root.mkString(separator)))
        |  def varying(name: String): Boolean = mutable.exists(root => name.startsWith(root.mkString("/")))
        |  def parameter(name: String, others: Vector[Vector[String]]): Boolean =
        |    others.exists(root => name.startsWith(root.mkString("/")))
        |  def trivial(name: String): Boolean = packages.exists(root => root.isEmpty)
        |  def element(name: String): Boolean = packages.exists(root => name.startsWith(root.mkString("/") + name))
        |}
        |object Constants {
        |  val packages = Vector(Vector("a"), Vector("b"))
        |  val total   = packages.exists(root => root.mkString("/").isEmpty)
        |}""".stripMargin
    )
    assertEquals(
      tagged(found, "invariant-in-loop"),
      List("root.mkString(\"/\") depends only on the elements of packages"),
      found.mkString("\n")
    )
  }

  for (
    (name, element, access) <- List(
      ("strings", "String", "_.take(1)"),
      ("vectors", "Vector[Int]", "_.take(1)"),
      ("lists", "List[Int]", "_.take(1)"),
      ("sets", "Set[Int]", "_.take(1)"),
      ("options", "Option[Int]", "_.filter(_ > 0)"),
      ("tuples", "(Int, String)", "_.swap")
    )
  )
    test(s"a lambda over immutable $name does not stop a traversal in a loop from being hoisted") {
      val found = warnings(
        s"""object Lints {
           |  def hoisted(parts: Vector[$element], n: Int): Vector[Int] =
           |    (0 until n).map(i => parts.map($access).size + i).toVector
           |}""".stripMargin
      )
      assertEquals(
        tagged(found, "invariant-in-loop"),
        List(s"parts.map($access) does not depend on the loop"),
        found.mkString("\n")
      )
    }

  test("a lambda that draws from a mutable buffer or an iterator keeps a traversal in a loop") {
    val found = warnings(
      """object Lints {
        |  def buffers(names: Vector[String], n: Int): Vector[Int] = {
        |    val taken = scala.collection.mutable.ArrayBuffer.empty[Int]
        |    (0 until n).map(i => names.map(name => taken.append(name.length)).size + i).toVector
        |  }
        |  def iterators(names: Vector[String], n: Int, source: Iterator[Int]): Vector[Int] =
        |    (0 until n).map(i => names.map(_ => source.next()).size + i).toVector
        |  def mutableElements(parts: Vector[scala.collection.mutable.ArrayBuffer[Int]], n: Int): Vector[Int] =
        |    (0 until n).map(i => parts.map(_.take(1)).size + i).toVector
        |}""".stripMargin
    )
    assertEquals(tagged(found, "invariant-in-loop"), Nil, found.mkString("\n"))
  }
}
