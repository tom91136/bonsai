package net.kurobako.bonsai.effects

class EffectSuite extends munit.FunSuite {
  private val base = Effect[String](Kind.Writes, "writes x", target = Some("local"), via = Some("a.b"))

  test("equal effects hash alike") {
    assertEquals(base.hashCode, base.copy().hashCode)
    assertEquals(
      base.hashCode,
      Effect[String](Kind.Writes, "writes x", target = Some("local"), via = Some("a.b")).hashCode
    )
  }

  test("every field takes part in the hash") {
    val changed = Seq(
      base.copy(kind = Kind.Reads),
      base.copy(cause = "writes y"),
      base.copy(onThis = true),
      base.copy(target = None),
      base.copy(via = None),
      base.copy(library = true),
      base.copy(assumed = true)
    )
    changed.foreach(effect => assertNotEquals(effect.hashCode, base.hashCode, effect.toString))
  }
}
