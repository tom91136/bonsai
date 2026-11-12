package net.kurobako.bonsai.analysis

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.core.TypeError
import dotty.tools.dotc.core.Types.JavaArrayType
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.core.Types.PolyType
import dotty.tools.dotc.core.Types.Type
import net.kurobako.bonsai.java.*
import net.kurobako.bonsai.lifting.*
import scala.reflect.NameTransformer

/** The key of a method as the javac plugin writes it: the qualified owner, the name and the erased JVM descriptor, so
  * that overloads do not collide and a Java caller and a Scala callee agree on a method.
  */
private object Keys {

  // Overloads, and a method named like a type beside it, are told apart by their parameter types.
  def signed(symbol: Symbol, name: String)(using Context): String = {
    val decls  = symbol.owner.info.decls
    val shared = symbol.is(Flags.Method) &&
      (decls.lookupAll(symbol.name).exists(_ != symbol) || decls.lookup(symbol.name.toTypeName).exists)
    if (shared) name + symbol.info.paramInfoss.map(_.map(_.show).mkString("(", ", ", ")")).mkString else name
  }

  /** The name of the method for display: signed when overloaded, and `#class` when the companion of its class also
    * declares it.
    */
  def shown(symbol: Symbol, name: String)(using Context): String = signed(symbol, name) + companioned(symbol)

  /** The key of the method, unless a class its signature names is missing from the classpath. */
  def of(method: Symbol)(using Context): Option[String] = at(method, Calls.owner(method))

  /** The key of the method, told apart from a method of the same name and signature in the companion of its class or in
    * another local class, which `of` gives the same key as class files do.
    */
  def unique(method: Symbol)(using Context): Option[String] = of(method).map(_ + qualifier(method))

  /** What tells a symbol from one of the same qualified name: `#class` for a member the companion of its class also
    * declares, and the position of a local definition within the member or class around it, which does not move when
    * code outside that member does.
    */
  def qualifier(symbol: Symbol)(using Context): String = companioned(symbol) + localised(symbol)

  private def companioned(symbol: Symbol)(using Context): String = {
    val owner = symbol.owner
    if (
      symbol.isTerm && owner.isClass && !owner.is(Flags.Module) &&
      owner.companionModule.moduleClass.info.decl(symbol.name).alternatives.exists(!_.symbol.is(Flags.Synthetic))
    )
      "#class"
    else ""
  }

  private def local(symbol: Symbol)(using Context): Boolean =
    symbol.ownersIterator.drop(1).exists(owner => owner.isTerm && !owner.is(Flags.Package))

  private def localised(symbol: Symbol)(using Context): String =
    if (local(symbol)) {
      val anchor = symbol.ownersIterator.drop(1).find(owner => owner.span.exists && !local(owner))
      val label  = anchor.fold("")(owner => signed(owner, owner.name.toString) + companioned(owner))
      s"@$label:${symbol.span.start - anchor.fold(0)(_.span.start)}"
    } else ""

  /** The key of a call of the method on a receiver of class `owner`, which is how class files name the callee. */
  def at(method: Symbol, owner: Symbol)(using Context): Option[String] =
    try {
      val (parameters, result) = signature(TypeErasure.transformInfo(method, method.info), Vector.empty)
      val returned             = if (method.isConstructor) Descriptors.primitive("void") else field(result)
      Some(Descriptors.key(
        Calls.qualified(owner),
        method.name.toString,
        parameters.map(field(_)),
        returned
      ))
    } catch { case _: TypeError => None }

  @annotation.tailrec
  private def signature(current: Type, parameters: Vector[Type]): (Vector[Type], Type) = current match {
    case method: MethodType => signature(method.resType, parameters ++ method.paramInfos)
    case poly: PolyType     => signature(poly.resType, parameters)
    case result             => (parameters, result)
  }

  private def field(tpe: Type)(using Context): String = {
    val widened = tpe.widen
    widened match {
      case JavaArrayType(element) => Descriptors.array(field(element))
      case _                      =>
        val cls = widened.classSymbol
        if (cls == defn.IntClass) Descriptors.primitive("int")
        else if (cls == defn.LongClass) Descriptors.primitive("long")
        else if (cls == defn.BooleanClass) Descriptors.primitive("boolean")
        else if (cls == defn.ByteClass) Descriptors.primitive("byte")
        else if (cls == defn.ShortClass) Descriptors.primitive("short")
        else if (cls == defn.CharClass) Descriptors.primitive("char")
        else if (cls == defn.FloatClass) Descriptors.primitive("float")
        else if (cls == defn.DoubleClass) Descriptors.primitive("double")
        else if (cls == defn.UnitClass) Descriptors.primitive("void")
        else if (cls.exists) Descriptors.clazz(binary(cls))
        else Descriptors.Object
    }
  }

  private[analysis] def binary(cls: Symbol)(using Context): String = {
    val owner = cls.owner
    val name  = NameTransformer.encode(cls.name.toString)
    if (owner.is(Flags.Package)) { if (owner.isEffectiveRoot) name else s"${binary(owner)}.$name" }
    else if (owner.isClass) { if (owner.is(Flags.Module)) binary(owner) + name else s"${binary(owner)}$$$name" }
    else name
  }

  private[analysis] def dotted(name: String): String = {
    @annotation.tailrec
    def go(index: Int, code: Boolean, operator: Boolean, out: String): String =
      if (index == name.length) out
      else
        name(index) match {
          case '/' | '.' => go(index + 1, code = true, operator = false, out = out + '.')
          case '$'       =>
            val prefix  = name.drop(index + 1).takeWhile(c => c != '$' && c != '.' && c != '/')
            val decoded = NameTransformer.decode(s"$$$prefix")
            if (index + 1 == name.length) out
            else if ((code || operator) && prefix.nonEmpty && decoded.length == 1)
              go(index + 1 + prefix.length, code = false, operator = true, out = out + decoded)
            else go(index + 1, code = true, operator = false, out = out + '.')
          case c => go(index + 1, code = false, operator = false, out = out + c)
        }
    go(0, code = true, operator = false, out = "")
  }
}
