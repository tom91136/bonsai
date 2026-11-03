package net.kurobako.bonsai.java

import javax.lang.model.element.ExecutableElement
import scala.collection.mutable

object Registry {
  val Returns = "returns"
  val Empty   = "empty"
}

/** What the lints of one compilation leave for each other: a lint that needs the whole hierarchy of a method records
  * what it saw as the compiler attributes the types, and reports at the end of the compilation, when it has seen them
  * all.
  */
class Registry(named: Option[Int] = None) {

  @SuppressWarnings(Array("scalafix:DisableSyntax.var"))
  private var probed = Option.empty[Int]

  /** The release the code is compiled for: the one the build names, or else the one `find` reads off the platform. */
  def release(find: => Int): Int = named.getOrElse {
    probed.getOrElse {
      val found = find
      probed = Some(found)
      found
    }
  }

  /** A method that has one trait, such as the value it returns or that its body is empty, or an abstract method, which
    * the end of the compilation reports if every method that overrides it has the trait too.
    */
  private case class Declared(
      kind: String,
      method: ExecutableElement,
      value: Option[String],
      report: String => Model.Lint,
      suppressed: Set[String] = Set.empty
  )

  private val declared   = mutable.ArrayBuffer.empty[Declared]
  private val overriders = mutable.Map.empty[(String, ExecutableElement), mutable.ArrayBuffer[Option[String]]]

  /** Records a method that overrides the methods, with the trait it has, if any. */
  def overrides(kind: String, parents: Iterable[ExecutableElement], value: Option[String]): Unit =
    parents.foreach(parent => overriders.getOrElseUpdate((kind, parent), mutable.ArrayBuffer.empty) += value)

  /** Records a method that overrides nothing, with the trait it has, if any. */
  def declares(
      kind: String,
      method: ExecutableElement,
      value: Option[String],
      report: String => Model.Lint,
      suppressed: Set[String]
  ): Unit =
    declared += Declared(kind, method, value, report, suppressed)

  /** The findings of the methods of the kind whose trait every override has too, which the end of the compilation
    * reports.
    */
  def agreed(kind: String, detail: String => String): Vector[Model.Lint] = {
    val kindId = if (kind == Registry.Empty) LintId.EmptyMethod.id else LintId.SameReturnValue.id
    declared.view.filter(d => d.kind == kind && !d.suppressed(LintId.Wildcard) && !d.suppressed(kindId)).flatMap {
      found =>
        val others = overriders.get((kind, found.method)).fold(Vector.empty[Option[String]])(_.toVector)
        (found.value.toVector.map(Option(_)) ++ others).distinct match {
          case Vector(Some(value)) if found.value.isDefined || others.nonEmpty => Some(found.report(detail(value)))
          case _                                                               => None
        }
    }.toVector
  }
}
