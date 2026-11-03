package net.kurobako.bonsai.collections

object Fixtures {
  val Strict: Kind = Kind(library = true)
  val Lazy: Kind   = Kind(library = true, strict = false)
  val User: Kind   = Kind(library = false)
  val Dict: Kind   = Kind(library = true, ordered = false, keyed = true)
  val Unique: Kind = Kind(library = true, ordered = false, distinct = true)
  val Opt: Kind    = Kind(library = true, single = true)

  def named(name: String, kind: Kind = Strict): Kind = kind.copy(name = name)

  val ListOf: Kind   = named("immutable.List")
  val VectorOf: Kind = named("immutable.Vector")
  val RangeOf: Kind  = named("immutable.Range")
  val SeqOf: Kind    = named("immutable.Seq")
  val ArrayOf: Kind  = named("Array")
  val StringOf: Kind = named("String")
  val SetOf: Kind    = named("immutable.Set", Unique)
  val MapOf: Kind    = named("immutable.Map", Dict)
  val OptionOf: Kind = named("Option", Opt)

  def equalTo(constant: String): Fn     = Fn.Compares(s"_ == $constant", equal = true, constant)
  def differsFrom(constant: String): Fn = Fn.Compares(s"_ != $constant", equal = false, constant)
  def member(set: String): Fn           = Fn.Member(s"$set.contains", set)

  def fn(text: String): Fn     = Fn.Plain(text, pure = true)
  def effect(text: String): Fn = Fn.Plain(text, pure = false)
  def literal[A](value: A): Fn = Fn.Constant(value)
  def projection(n: Int): Fn   = Fn.Projected(n)

  val pair: String = "(_, _)"

  def reading(shape: String): Fn  = Fn.Facts("_._1 > 0", inspects = Some(shape))
  def matching(shape: String): Fn = Fn.Facts(s"{ case $shape => }", Some(shape), Some(shape))
  def casting(tpe: String): Fn    = Fn.Facts(s"_.asInstanceOf[$tpe]", destructures = Some(s"_: $tpe"))
  def selecting(text: String): Fn = Fn.Facts(text, selection = true)
  def diverging(text: String): Fn = Fn.Facts(text, diverges = true)
  val same: Fn                    = Fn.Called("identity", value => value, identity = true)

  def step(method: String, args: Arg*): Step = Step(method, args.toVector, Strict, Strict)

  def on(kind: Kind, method: String, args: Arg*): Step = Step(method, args.toVector, kind, kind)

  def chain(source: String, steps: Step*): Chain = Chain(fn(source), steps.toVector)

  def rewrites(chain: Chain): Vector[Outcome.Rewrite] = Laws.outcomes(chain).collect { case r: Outcome.Rewrite => r }

  def reports(chain: Chain): Vector[Outcome.Report] = Laws.outcomes(chain).collect { case r: Outcome.Report => r }
}
