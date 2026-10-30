package net.kurobako.baumkuchen.java

import com.sun.source.tree.*
import com.sun.source.util.TreeScanner
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.TypeElement
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Lints of the log messages of a file. */
object LintLogging {

  val lints: Vector[Lint] = Vector(SimilarMessages)

  /** Two log calls whose messages say nearly the same thing. */
  object SimilarMessages extends Lint {
    val id    = JavaLint.LoggingSimilar.id
    val kinds = Set.empty[Tree.Kind]

    private val Levels = Set("trace", "debug", "info", "warn", "error")

    def check(context: Context, tree: Tree): Vector[Found] = {
      val messages = mutable.ArrayBuffer.empty[(String, Tree)]
      new TreeScanner[Unit, Unit] {
        override def visitMethodInvocation(node: MethodInvocationTree, unused: Unit): Unit = {
          context.trees.getElement(context.trees.getPath(context.unit, node)) match {
            case method: ExecutableElement
                if Levels(method.getSimpleName.toString) &&
                  Option(method.getEnclosingElement).exists {
                    case owner: TypeElement => owner.getQualifiedName.contentEquals("org.slf4j.Logger")
                    case _                  => false
                  } =>
              node.getArguments.asScala.headOption.collect {
                case literal: LiteralTree => Option(literal.getValue).collect { case text: String => text }
              }.flatten.foreach(text => messages += ((text, node)))
            case _ => ()
          }
          super.visitMethodInvocation(node, unused)
        }
      }.scan(context.unit, ())

      messages.zipWithIndex.flatMap { case ((text, node), index) =>
        messages.take(index).collectFirst {
          case (other, _) if similar(text, other) =>
            context.found(s"the log message is similar to another message in this file: $other", node)
        }
      }.toVector
    }

    private def similar(left: String, right: String): Boolean = {
      val a = words(left)
      val b = words(right)
      a.nonEmpty && b.nonEmpty && (a.intersect(b).size.toDouble / a.union(b).size) >= 0.8
    }

    private def words(text: String): Set[String] =
      text.toLowerCase.split("[^a-z0-9]+").filter(_.nonEmpty).toSet
  }
}
