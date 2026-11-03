package net.kurobako.bonsai.lifting

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ContextBase
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.core.Symbols.requiredClass
import java.lang.ref.WeakReference
import scala.collection.mutable

/** Classes of the compiler's classpath by name, looked up once for each compilation. */
object Classes {
  // The compilation holds its classes, so neither its entry nor the classes keep it alive.
  private val cache = ThreadLocal.withInitial(() =>
    java.util.WeakHashMap[ContextBase, mutable.HashMap[String, WeakReference[ClassSymbol]]]()
  )

  def required(name: String)(using Context): ClassSymbol = {
    val known = cache.get().computeIfAbsent(ctx.base, _ => mutable.HashMap.empty)
    known.get(name).flatMap(reference => Option(reference.get)).getOrElse {
      val found = requiredClass(name)
      known(name) = WeakReference(found)
      found
    }
  }
}
