package net.kurobako.bonsai.plugin

import Warnings.*
import munit.FunSuite

class RepeatsSuite extends FunSuite {
  private def repeated(source: String): List[String] = tagged(Warnings.of(source), "repeated-subexpression")

  test("the same chain of library reads on stable roots, twice or more in a method, is reported once") {
    val found = repeated(
      """case class Fragment(family: String, lines: Vector[String])
        |case class Group(fragments: Vector[Fragment])
        |object Lints {
        |  def clash(group: Group): Boolean =
        |    group.fragments.map(_.family).distinct.size > 1 && group.fragments.map(_.family).distinct.head.nonEmpty
        |  def normalised(text: String): String = {
        |    val key = text.trim.toLowerCase
        |    if (key.isEmpty) text.trim.toLowerCase else key + text.trim.toLowerCase
        |  }
        |  def later(group: Group, flag: Boolean): Int =
        |    (if (flag) group.fragments.map(_.family).distinct.size else 0) + group.fragments.map(_.family).distinct.size
        |  def thrice(group: Group): Int = {
        |    val first  = group.fragments.filter(_.lines.nonEmpty).map(_.family).size
        |    val second = group.fragments.filter(_.lines.nonEmpty).map(_.family).size
        |    val third  = group.fragments.filter(_.lines.nonEmpty).map(_.family).size
        |    first + second + third
        |  }
        |}""".stripMargin
    )
    assertEquals(
      found,
      List(
        "group.fragments.map(_.family).distinct is evaluated 2 times in clash",
        "group.fragments.map(_.family).distinct.size is evaluated 2 times in later",
        "text.trim.toLowerCase is evaluated 3 times in normalised",
        "group.fragments.filter(_.lines.nonEmpty).map(_.family).size is evaluated 3 times in thrice"
      ).sorted
    )
  }

  test("a repetition inside lambdas that bind their own parameters is not the same chain") {
    val found = repeated(
      """case class Entry(name: String)
        |object Lints {
        |  def both(xs: Vector[Entry], ys: Vector[Entry]): Int =
        |    xs.map(x => x.name.trim.toLowerCase).size + ys.map(y => y.name.trim.toLowerCase).size
        |  def nested(xs: Vector[Entry]): Vector[Int] =
        |    xs.map(x => x.name.trim.toLowerCase.length + xs.map(_.name.trim.toLowerCase).size)
        |}""".stripMargin
    )
    assertEquals(found, Nil)
  }

  test("a chain that is read once, is a single call, or sits in exclusive branches is not reported") {
    val found = repeated(
      """case class Group(names: Vector[String])
        |object Lints {
        |  def once(group: Group): Int = group.names.map(_.trim).distinct.size
        |  def single(group: Group): Int = group.names.toSet.size + group.names.toSet.size
        |  def exclusive(group: Group, flag: Boolean): Int =
        |    if (flag) group.names.map(_.trim).distinct.size else group.names.map(_.trim).distinct.length
        |  def matched(group: Group, key: Int): Int = key match {
        |    case 0 => group.names.map(_.trim).distinct.size
        |    case _ => group.names.map(_.trim).distinct.size + 1
        |  }
        |  def guarded(group: Group, flag: Boolean): Int =
        |    if (flag) group.names.map(_.trim).distinct.size else 0
        |  def lazily(group: Group, flag: Boolean): Boolean =
        |    flag && group.names.map(_.trim).distinct.nonEmpty || group.names.map(_.trim).distinct.isEmpty
        |  def separate(group: Group): Int = {
        |    def inner(): Int = group.names.map(_.trim).distinct.size
        |    inner() + group.names.map(_.trim).distinct.size
        |  }
        |}""".stripMargin
    )
    assertEquals(found, Nil)
  }

  test("chains that read variables, mutable collections, iterators or user code are not pure reads") {
    val found = repeated(
      """import scala.collection.mutable
        |object Lints {
        |  var names = Vector("a")
        |  def variable(): Int = names.map(_.trim).distinct.size + names.map(_.trim).distinct.size
        |  def buffer(items: mutable.ArrayBuffer[String]): Int = items.map(_.trim).toList.size + items.map(_.trim).toList.size
        |  def iterated(items: Vector[String]): Int =
        |    items.iterator.map(_.trim).toList.size + items.iterator.map(_.trim).toList.size
        |  def array(items: Array[String]): Int = items.map(_.trim).toList.size + items.map(_.trim).toList.size
        |  def user(items: Vector[String]): Int = load(items).map(_.trim).size + load(items).map(_.trim).size
        |  def assigned(items: Vector[String]): Int = {
        |    var local = items
        |    val before = local.map(_.trim).distinct.size
        |    local = local.drop(1)
        |    before + local.map(_.trim).distinct.size
        |  }
        |  def random(items: Vector[String], seed: scala.util.Random): Int =
        |    items.map(_ => seed.nextInt()).sum + items.map(_ => seed.nextInt()).sum
        |  def effect(items: Vector[String]): Int =
        |    items.map(item => { println(item); item }).size + items.map(item => { println(item); item }).size
        |  private def load(items: Vector[String]): Vector[String] = items
        |}""".stripMargin
    )
    assertEquals(found, Nil)
  }
}
