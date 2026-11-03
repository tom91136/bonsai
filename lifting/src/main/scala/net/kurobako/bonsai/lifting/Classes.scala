package net.kurobako.bonsai.lifting

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ContextBase
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.getClassIfDefined
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.dotc.core.Types.Type
import java.lang.ref.WeakReference
import scala.collection.mutable

/** Classes of the compiler's classpath by name, looked up once for each compilation. */
object Classes {
  // The compilation holds its classes, so neither its entry nor the classes keep it alive.
  private val cache = ThreadLocal.withInitial(() =>
    java.util.WeakHashMap[ContextBase, mutable.HashMap[String, WeakReference[Symbol]]]()
  )

  def required(name: String)(using Context): ClassSymbol = lookup(name, requiredClass(name)).asClass

  def named(name: String)(using Context): Symbol = lookup(name, getClassIfDefined(name))

  def derives(tpe: Type, names: String*)(using Context): Boolean =
    names.exists(name => tpe.derivesFrom(required(name)))

  private def lookup(name: String, find: => Symbol)(using Context): Symbol = {
    val known = cache.get().computeIfAbsent(ctx.base, _ => mutable.HashMap.empty)
    known.get(name).flatMap(reference => Option(reference.get)).getOrElse {
      val found = find
      known(name) = WeakReference(found)
      found
    }
  }
}
