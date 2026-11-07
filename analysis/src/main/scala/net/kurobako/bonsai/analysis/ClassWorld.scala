package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.analysis.ClassFiles.*
import net.kurobako.bonsai.analysis.ClassWorld.*
import org.objectweb.asm.Opcodes
import scala.collection.mutable

private[analysis] object ClassWorld {
  case class Target(owner: String, method: Method)

  def apply(load: String => Option[Array[Byte]]): ClassWorld = new ClassWorld(load)
}

private[analysis] class ClassWorld(load: String => Option[Array[Byte]]) {
  private val objectClass     = "java/lang/Object"
  private val classes         = mutable.Map.empty[String, Option[ClassFile]]
  private val ancestries      = mutable.Map.empty[String, Vector[String]]
  private val chains          = mutable.Map.empty[String, Vector[String]]
  private val resolved        = mutable.Map.empty[Reference, Option[Target]]
  private val indices         = mutable.Map.empty[String, Map[String, Vector[Method]]]
  private val unreadableNames = mutable.Map.empty[String, String]

  def unreadable: Map[String, String] = unreadableNames.toMap

  def get(name: String): Option[ClassFile] =
    if (name.startsWith("[")) Some(array(name)) else classes.getOrElseUpdate(name, read(name))

  def ancestors(name: String): Vector[String] = ancestries.getOrElseUpdate(
    name, {
      val chain = superclasses(name)
      chain ++ closure((name +: chain).flatMap(get(_)).flatMap(_.interfaces), Vector.empty)
    }
  )

  def key(reference: Reference): String =
    ClassFiles.key(reference, get(reference.owner).flatMap(_.enclosingInstance))

  /** The class that declares the field `access` names, found by the order of JVMS 5.4.3.2. */
  def fieldHolder(access: FieldAccess): Option[String] = holder(access.owner, access)

  private def holder(owner: String, access: FieldAccess): Option[String] =
    get(owner).flatMap { file =>
      if (file.fields.exists(field => field.name == access.name && field.descriptor == access.descriptor)) Some(owner)
      else
        file.interfaces.view.flatMap(holder(_, access)).headOption
          .orElse(file.superName.flatMap(holder(_, access)))
    }

  /** Whether `owner` declares a method that a subclass can override and that has the name and descriptor of `method`.
    */
  def declares(owner: String, method: Method): Boolean =
    declared(owner, method.name, method.descriptor).exists(_.method.overridable)

  def resolve(reference: Reference): Option[Target] =
    resolved.getOrElseUpdate(reference, lookup(reference.owner, reference.name, reference.descriptor))

  def select(receiver: String, reference: Reference): Option[Target] =
    resolve(reference).flatMap { target =>
      val method = target.method
      if (method.overridable) dispatch(receiver, method.name, method.descriptor) else Some(target)
    }

  private def lookup(owner: String, name: String, descriptor: String): Option[Target] = {
    val inClasses = (owner +: superclasses(owner)).view.flatMap(declared(_, name, descriptor))
    inClasses.headOption.orElse(pick(inherited(owner, name, descriptor)))
  }

  private def dispatch(receiver: String, name: String, descriptor: String): Option[Target] = {
    val inClasses = (receiver +: superclasses(receiver)).view.flatMap(declared(_, name, descriptor))
    inClasses.find(target => target.method.overridable).orElse(pick(inherited(receiver, name, descriptor)))
  }

  private def read(name: String): Option[ClassFile] =
    load(name) match {
      case None        => None
      case Some(bytes) =>
        ClassFiles.read(bytes) match {
          case Right(file) => Some(file)
          case Left(error) =>
            unreadableNames(name) = error
            None
        }
    }

  private def superclasses(name: String): Vector[String] = chains.getOrElseUpdate(
    name, {
      @annotation.tailrec
      def climb(current: String, acc: Vector[String]): Vector[String] =
        get(current).flatMap(_.superName) match {
          case Some(parent) => climb(parent, acc :+ parent)
          case None         => acc
        }
      climb(name, Vector.empty)
    }
  )

  @annotation.tailrec
  private def closure(pending: Vector[String], seen: Vector[String]): Vector[String] =
    pending match {
      case head +: rest if seen.contains(head) => closure(rest, seen)
      case head +: rest => closure(rest ++ get(head).toVector.flatMap(_.interfaces), seen :+ head)
      case _            => seen
    }

  private def array(name: String): ClassFile =
    ClassFile(name, Opcodes.ACC_PUBLIC, Some(objectClass), Vector.empty, Vector.empty, Vector.empty, None)

  private def declared(owner: String, name: String, descriptor: String): Option[Target] = {
    val named = indices.getOrElseUpdate(owner, get(owner).fold(Map.empty)(_.methods.groupBy(_.name))).get(name)
    named.flatMap(_.find(matches(owner, _, descriptor))).map(Target(owner, _))
  }

  private def matches(owner: String, method: Method, descriptor: String): Boolean =
    method.descriptor == descriptor || polymorphic(owner, method)

  private def inherited(owner: String, name: String, descriptor: String): Vector[Target] = {
    val interfaces = ancestors(owner).filter(isInterface(_))
    val candidates = interfaces.flatMap(declared(_, name, descriptor)).filter(target => target.method.overridable)
    val superseded = candidates.flatMap(candidate => ancestors(candidate.owner)).toSet
    candidates.filterNot(candidate => superseded.contains(candidate.owner))
  }

  private def pick(candidates: Vector[Target]): Option[Target] =
    candidates.find(target => !target.method.isAbstract).orElse(candidates.headOption)

  private def isInterface(name: String): Boolean = get(name).exists(_.isInterface)

  private def polymorphic(owner: String, method: Method): Boolean =
    (owner == "java/lang/invoke/MethodHandle" || owner == "java/lang/invoke/VarHandle") &&
      method.isNative && method.isVarargs && method.descriptor.startsWith("([Ljava/lang/Object;)")
}
