package net.kurobako.bonsai.effects

import net.kurobako.bonsai.effects.Partials.Partial

class PartialsSuite extends munit.FunSuite {
  test("partial calls and their alternatives") {
    val expected = Map(
      "scala.collection.MapOps.apply"           -> Partial("get", true),
      "scala.collection.SeqOps.apply"           -> Partial("lift", true),
      "scala.collection.IterableOps.head"       -> Partial("headOption", true),
      "scala.collection.IterableOps.last"       -> Partial("lastOption", true),
      "scala.collection.IterableOnceOps.reduce" -> Partial("reduceOption", true),
      "scala.collection.IterableOnceOps.max"    -> Partial("maxOption", true),
      "scala.collection.IterableOnceOps.min"    -> Partial("minOption", true),
      "scala.Option.get"                        -> Partial("getOrElse or fold", false),
      "scala.collection.StringOps.toInt"        -> Partial("toIntOption", true),
      "scala.collection.StringOps.toLong"       -> Partial("toLongOption", true),
      "scala.collection.StringOps.toDouble"     -> Partial("toDoubleOption", true),
      "scala.collection.StringOps.toFloat"      -> Partial("toFloatOption", true),
      "scala.collection.StringOps.toShort"      -> Partial("toShortOption", true),
      "scala.collection.StringOps.toByte"       -> Partial("toByteOption", true),
      "scala.collection.StringOps.toBoolean"    -> Partial("toBooleanOption", true),
      "java.lang.Integer.parseInt"              -> Partial("toIntOption", false),
      "java.lang.Long.parseLong"                -> Partial("toLongOption", false),
      "ujson.Value.obj"                         -> Partial("objOpt", true),
      "ujson.Value.str"                         -> Partial("strOpt", true),
      "ujson.Value.num"                         -> Partial("numOpt", true),
      "ujson.Value.arr"                         -> Partial("arrOpt", true),
      "ujson.Value.bool"                        -> Partial("boolOpt", true),
      "ujson.Value.apply"                       -> Partial("objOpt and arrOpt lookups", false)
    )
    assertEquals(Partials.Alternatives, expected)
  }

  test("the names of the standard library that fail for some inputs") {
    assertEquals(
      Partials.Names,
      Set(
        "head",
        "last",
        "init",
        "tail",
        "apply",
        "get",
        "reduce",
        "reduceLeft",
        "reduceRight",
        "max",
        "min",
        "maxBy",
        "minBy",
        "toInt",
        "toLong",
        "toDouble",
        "toFloat",
        "toShort",
        "toByte",
        "toBoolean",
        "parseInt",
        "parseLong",
        "require",
        "assert",
        "assume",
        "ensuring",
        "/",
        "%",
        "<init>"
      )
    )
  }
}
