package net.kurobako.bonsai.java

import com.sun.source.tree.*
import com.sun.source.tree.Tree.Kind
import com.sun.source.util.TreeScanner
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.TypeElement
import scala.collection.mutable

/** Lints of the log messages of a file. */
object LintLogging {

  val lints: Vector[Lint] = Vector(SimilarMessages)

  /** Two log calls whose messages say nearly the same thing. */
  private object SimilarMessages extends Lint {
    val id               = JavaLint.LoggingSimilar.id
    val kinds: Set[Kind] = Set.empty[Tree.Kind]

    private val Levels = Set("trace", "debug", "info", "warn", "error")

    def check(context: Context, tree: Tree): Vector[Found] = {
      val messages = mutable.ArrayBuffer.empty[(String, Tree)]
      new TreeScanner[Unit, Unit] {
        override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
          context.elementOf(node) match {
            case method: ExecutableElement
                if Levels(method.getSimpleName.toString) &&
                  Option(method.getEnclosingElement).exists {
                    case owner: TypeElement => owner.getQualifiedName.contentEquals("org.slf4j.Logger")
                    case _                  => false
                  } =>
              Lints.arguments(node).headOption.flatMap(Lints.literalText).foreach(text =>
                messages += ((text, node))
              )
            case _ => ()
          }
          super.visitMethodInvocation(node, unused)
        }
      }.scan(context.unit, ())

      val tokenised = messages.map { case (text, node) => (text, node, words(text)) }
      tokenised.zipWithIndex.flatMap { case ((_, node, mine), index) =>
        tokenised.take(index).collectFirst {
          case (other, _, theirs) if similar(mine, theirs) =>
            context.found(s"the log message is similar to another message in this file: $other", node)
        }
      }.toVector
    }

    private def similar(a: Set[String], b: Set[String]): Boolean =
      a.nonEmpty && b.nonEmpty && (a.intersect(b).size.toDouble / a.union(b).size) >= 0.8

    private def words(text: String): Set[String] =
      text.toLowerCase.split("[^a-z0-9]+").filter(_.nonEmpty).toSet
  }
}
