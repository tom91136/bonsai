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

/** The key of a method as the javac plugin writes it: the qualified owner, the name and the erased JVM descriptor, so
  * that overloads do not collide and a Java caller and a Scala callee agree on a method.
  */
private object Keys {

  /** The key of the method, unless a class its signature names is missing from the classpath. */
  def of(method: Symbol)(using Context): Option[String] = at(method, Calls.owner(method))

  /** The key of the method, told apart from a method of the same name and signature in the companion of its class or in
    * another local class, which `of` gives the same key as class files do.
    */
  def unique(method: Symbol)(using Context): Option[String] =
    of(method).map(_ + companioned(method) + localised(method))

  private def companioned(method: Symbol)(using Context): String = {
    val owner = method.owner
    if (owner.isClass && !owner.is(Flags.Module) && owner.companionModule.moduleClass.info.decl(method.name).exists)
      "#class"
    else ""
  }

  private def localised(method: Symbol)(using Context): String =
    if (method.ownersIterator.drop(1).exists(owner => owner.isTerm && !owner.is(Flags.Package)))
      s"@${method.span.start}"
    else ""

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

  private def binary(cls: Symbol)(using Context): String = {
    val owner = cls.owner
    val name  = cls.name.toString
    if (owner.is(Flags.Package)) { if (owner.isEffectiveRoot) name else s"${owner.fullName}.$name" }
    else if (owner.isClass) { if (owner.is(Flags.Module)) binary(owner) + name else s"${binary(owner)}$$$name" }
    else name
  }
}
