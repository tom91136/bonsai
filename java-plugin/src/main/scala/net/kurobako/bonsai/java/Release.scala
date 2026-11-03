package net.kurobako.bonsai.java

/** The Java release the code is compiled for, which decides what a lint may suggest: a call or a construct that a newer
  * release introduced is no advice for a module that compiles for an older one.
  *
  * The compiler shows the classes of the release it compiles for, so the release is read off the newest class of the
  * platform the code can see, unless the build names it with `release=N` among the arguments of the plugin, as it must
  * for a module that sets its language level without a release.
  */
object Release {

  /** The classes or methods, written `Type` or `Type#method`, that the releases introduced, the oldest first. */
  private val Markers = Vector(
    9  -> "java.lang.StackWalker",
    10 -> "java.util.List#copyOf",
    11 -> "java.lang.String#isBlank",
    12 -> "java.lang.constant.Constable",
    15 -> "java.lang.CharSequence#isEmpty",
    16 -> "java.lang.Record",
    17 -> "java.util.random.RandomGenerator",
    19 -> "java.lang.WrongThreadException",
    21 -> "java.util.SequencedCollection",
    22 -> "java.lang.foreign.Arena",
    25 -> "java.lang.ScopedValue"
  )

  /** The newest release whose class the code can see, or 8 when it can see none of them. */
  private def probe(context: Context): Int =
    Markers.foldLeft(8) { case (found, (release, marker)) =>
      val (name, member) = marker.span(_ != '#')
      val present        = Option(context.elements.getTypeElement(name)).exists { tpe =>
        member.isEmpty ||
        tpe.getEnclosedElements.stream().anyMatch(element => element.getSimpleName.contentEquals(member.drop(1)))
      }
      if (present) release else found
    }

  /** Whether the code may use what the release introduced. */
  def atLeast(context: Context, release: Int): Boolean = context.registry.release(probe(context)) >= release
}
