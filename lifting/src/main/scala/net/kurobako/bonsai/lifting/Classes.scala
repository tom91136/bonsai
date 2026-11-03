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

/** Classes of the compiler's classpath by name, looked up once for each compilation. */
object Classes {
  // The compilation holds its classes, so neither its entry nor the classes keep it alive. A thread keeps its value
  // until it next reads the cache, so the values are of the JDK alone and hold no class of a compiler to unload.
  private val cache = ThreadLocal.withInitial(() =>
    java.util.WeakHashMap[ContextBase, java.util.HashMap[String, WeakReference[Symbol]]]()
  )

  def required(name: String)(using Context): ClassSymbol = lookup(name, requiredClass(name)).asClass

  def named(name: String)(using Context): Symbol = lookup(name, getClassIfDefined(name))

  def derives(tpe: Type, names: String*)(using Context): Boolean =
    names.exists(name => tpe.derivesFrom(required(name)))

  private def known(using Context): java.util.HashMap[String, WeakReference[Symbol]] =
    cache.get().computeIfAbsent(ctx.base, _ => java.util.HashMap())

  // a miss is null, which keeps a hit free of wrappers
  private def cached(name: String)(using Context): Symbol = {
    val reference = known.get(name)
    if (reference == null) null else reference.get // scalafix:ok DisableSyntax.null
  }

  private def remember(name: String, found: Symbol)(using Context): Symbol = {
    known.put(name, WeakReference(found)): Unit
    found
  }

  private inline def lookup(name: String, inline find: Symbol)(using Context): Symbol = {
    val hit = cached(name)
    if (hit == null) remember(name, find) else hit // scalafix:ok DisableSyntax.null
  }
}
