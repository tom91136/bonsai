package net.kurobako.bonsai.analysis

import java.lang.invoke.LambdaMetafactory
import org.objectweb.asm.Attribute
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.Handle
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type
import org.objectweb.asm.tree.MethodNode
import scala.collection.mutable
import scala.reflect.NameTransformer

private[analysis] object ClassFiles {
  enum Kind {
    case Virtual, Interface, Static, Special
  }

  case class Invocation(kind: Kind, owner: String, name: String, descriptor: String)

  case class Lambda(
      interface: String,
      sam: String,
      samDescriptor: String,
      markers: Vector[String],
      bridges: Vector[String],
      owner: String,
      name: String,
      descriptor: String,
      tag: Int
  )

  case class Dynamic(owner: String, name: String)

  case class FieldAccess(owner: String, name: String, descriptor: String, static: Boolean, write: Boolean)

  case class Field(name: String, descriptor: String, access: Int) {
    def isStatic: Boolean = (access & Opcodes.ACC_STATIC) != 0
  }

  case class Reference(owner: String, name: String, descriptor: String) {
    override lazy val hashCode: Int = (owner.hashCode * 31 + name.hashCode) * 31 + descriptor.hashCode

    override def equals(other: Any): Boolean = other match {
      case that: Reference => hashCode == that.hashCode && owner == that.owner && name == that.name &&
        descriptor == that.descriptor
      case _ => false
    }
  }

  case class Method(
      name: String,
      descriptor: String,
      access: Int,
      invocations: Vector[Invocation],
      instantiated: Set[String],
      lambdas: Vector[Lambda],
      dynamics: Vector[Dynamic],
      fields: Vector[FieldAccess]
  ) {
    def isStatic: Boolean    = (access & Opcodes.ACC_STATIC) != 0
    def overridable: Boolean = (access & (Opcodes.ACC_STATIC | Opcodes.ACC_PRIVATE)) == 0
    def isAbstract: Boolean  = (access & Opcodes.ACC_ABSTRACT) != 0
    def isNative: Boolean    = (access & Opcodes.ACC_NATIVE) != 0
    def isVarargs: Boolean   = (access & Opcodes.ACC_VARARGS) != 0
  }

  case class ClassFile(
      name: String,
      access: Int,
      superName: Option[String],
      interfaces: Vector[String],
      methods: Vector[Method],
      fields: Vector[Field],
      enclosingInstance: Option[String],
      sourceFile: Option[String] = None,
      outer: Option[String] = None,
      tasty: Boolean = false
  ) {
    def isInterface: Boolean = (access & Opcodes.ACC_INTERFACE) != 0
    def isAbstract: Boolean  = (access & Opcodes.ACC_ABSTRACT) != 0
  }

  def read(bytes: Array[Byte]): Either[String, ClassFile] =
    try {
      val visitor = Visitor()
      ClassReader(bytes).accept(visitor, Array[Attribute](TastyAttribute), ClassReader.SKIP_FRAMES)
      Right(visitor.file)
    } catch {
      case error @ (_: IllegalArgumentException | _: IndexOutOfBoundsException) => Left(error.toString)
    }

  @SuppressWarnings(Array("bonsai:long-arguments"))
  private def bodyNode(access: Int, name: String, descriptor: String): MethodNode =
    MethodNode(Opcodes.ASM9, access, name, descriptor, null, null) // scalafix:ok DisableSyntax.null

  private class Selector(wanted: ((String, String)) => Boolean, selected: mutable.ArrayBuffer[MethodNode])
      extends ClassVisitor(Opcodes.ASM9) {
    override def visitMethod(
        access: Int,
        name: String,
        descriptor: String,
        signature: String,
        exceptions: Array[String]
    ): MethodVisitor =
      if (wanted((name, descriptor))) {
        val node = bodyNode(access, name, descriptor)
        selected += node
        node
      } else null // scalafix:ok DisableSyntax.null
  }

  /** The body of the method `name` with `descriptor` that the class `bytes` declares, when it has code. */
  @SuppressWarnings(Array("bonsai:test-only-member"))
  def body(bytes: Array[Byte], name: String, descriptor: String): Option[MethodNode] = {
    val selected = mutable.ArrayBuffer.empty[MethodNode]
    ClassReader(bytes).accept(
      Selector(_ == ((name, descriptor)), selected),
      ClassReader.SKIP_FRAMES | ClassReader.SKIP_DEBUG
    )
    selected.headOption.filter(_.instructions.size > 0)
  }

  /** What the `methods` of the class `bytes` hold do to the objects they may not own, by name and descriptor. */
  def trace(bytes: Array[Byte], owner: String, methods: Vector[Method]): Map[(String, String), Trace] = {
    val wanted   = methods.map(method => (method.name, method.descriptor) -> method).toMap
    val selected = mutable.ArrayBuffer.empty[MethodNode]
    ClassReader(bytes).accept(Selector(wanted.contains, selected), ClassReader.SKIP_FRAMES | ClassReader.SKIP_DEBUG)
    selected.view.map { node =>
      val method = wanted((node.name, node.desc))
      (node.name, node.desc) ->
        (if (node.instructions.size == 0) Provenance.none
         else Provenance.of(owner, node, method.invocations.size, method.fields.size))
    }.toMap
  }

  /** The key `Keys` gives the method. A constructor of an inner class drops the enclosing instance that the class file
    * passes first and the source does not name.
    */
  def key(reference: Reference, enclosingInstance: Option[String]): String = {
    val binary  = reference.owner.replace('/', '.').replace('$', '.').stripSuffix(".")
    val simple  = if (reference.name == "<init>") reference.name else NameTransformer.decode(reference.name)
    val outer   = enclosingInstance.map(name => s"(L$name;").filter(_ => reference.name == "<init>")
    val dropped = outer.filter(reference.descriptor.startsWith(_)).fold(reference.descriptor)(prefix =>
      "(" + reference.descriptor.stripPrefix(prefix)
    )
    s"$binary.$simple$dropped"
  }

  /** Marks the class files of Scala 3 code, which TASTy describes better than the bytecode does. */
  private object TastyAttribute extends Attribute("TASTY")

  private val lambdaMetafactory = "java/lang/invoke/LambdaMetafactory"

  private def extras(arguments: Vector[Any], bootstrap: String): (Vector[String], Vector[String]) =
    arguments.lift(3) match {
      case Some(flags: Int) if bootstrap == "altMetafactory" =>
        val (markers, rest) =
          if ((flags & LambdaMetafactory.FLAG_MARKERS) != 0) counted(arguments.drop(4))
          else (Vector.empty, arguments.drop(4))
        val bridges = if ((flags & LambdaMetafactory.FLAG_BRIDGES) != 0) counted(rest)._1 else Vector.empty
        (markers.map(_.getInternalName), bridges.map(_.getDescriptor))
      case _ => (Vector.empty, Vector.empty)
    }

  private def counted(arguments: Vector[Any]): (Vector[Type], Vector[Any]) = arguments match {
    case (count: Int) +: rest => (rest.take(count).collect { case tpe: Type => tpe }, rest.drop(count))
    case _                    => (Vector.empty, arguments)
  }

  /** The erased functional interface method and the implementation of a `LambdaMetafactory` call site. */
  def lambdaSite(bootstrap: Handle, arguments: Seq[AnyRef]): Option[(Type, Handle)] =
    arguments match {
      case Seq(sam: Type, implementation: Handle, _*) if bootstrap.getOwner == lambdaMetafactory =>
        Some((sam, implementation))
      case _ => None
    }

  def kindOf(opcode: Int): Kind = opcode match {
    case Opcodes.INVOKEVIRTUAL   => Kind.Virtual
    case Opcodes.INVOKEINTERFACE => Kind.Interface
    case Opcodes.INVOKESTATIC    => Kind.Static
    case _                       => Kind.Special
  }

  private class Visitor extends ClassVisitor(Opcodes.ASM9) {
    private var header = ClassFile( // scalafix:ok DisableSyntax.var
      name = "",
      access = 0,
      superName = None,
      interfaces = Vector.empty,
      methods = Vector.empty,
      fields = Vector.empty,
      enclosingInstance = None
    )
    private val methods = mutable.ArrayBuffer.empty[Method]
    private val fields  = mutable.ArrayBuffer.empty[Field]

    override def visit(
        version: Int,
        access: Int,
        name: String,
        signature: String,
        superName: String,
        interfaces: Array[String]
    ): Unit =
      header = header.copy(
        name = name,
        access = access,
        superName = Option(superName),
        interfaces = interfaces.toVector
      )

    override def visitSource(source: String, debug: String): Unit =
      header = header.copy(sourceFile = Option(source))

    override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int): Unit =
      if (name == header.name) {
        Option(outerName).foreach(outer => header = header.copy(outer = Some(outer)))
        if ((access & Opcodes.ACC_STATIC) == 0)
          Option(outerName).foreach(outer => header = header.copy(enclosingInstance = Some(outer)))
      }

    override def visitOuterClass(owner: String, name: String, descriptor: String): Unit =
      header = header.copy(outer = Some(owner))

    override def visitAttribute(attribute: Attribute): Unit =
      if (attribute.`type` == "TASTY") header = header.copy(tasty = true)

    override def visitField(
        access: Int,
        name: String,
        descriptor: String,
        signature: String,
        value: AnyRef
    ): FieldVisitor = {
      fields += Field(name, descriptor, access): Unit
      null // scalafix:ok DisableSyntax.null
    }

    override def visitMethod(
        access: Int,
        name: String,
        descriptor: String,
        signature: String,
        exceptions: Array[String]
    ): MethodVisitor =
      Body(methods, access, name, descriptor)

    def file: ClassFile = header.copy(methods = methods.toVector, fields = fields.toVector)
  }

  private class Body(sink: mutable.ArrayBuffer[Method], access: Int, name: String, descriptor: String)
      extends MethodVisitor(Opcodes.ASM9) {
    private val invocations  = mutable.ArrayBuffer.empty[Invocation]
    private val instantiated = mutable.Set.empty[String]
    private val lambdas      = mutable.ArrayBuffer.empty[Lambda]
    private val dynamics     = mutable.ArrayBuffer.empty[Dynamic]
    private val fields       = mutable.ArrayBuffer.empty[FieldAccess]

    override def visitTypeInsn(opcode: Int, internalName: String): Unit =
      if (opcode == Opcodes.NEW) instantiated += internalName: Unit

    override def visitMethodInsn(
        opcode: Int,
        owner: String,
        name: String,
        descriptor: String,
        isInterface: Boolean
    ): Unit =
      invocations += Invocation(kindOf(opcode), owner, name, descriptor): Unit

    override def visitFieldInsn(opcode: Int, owner: String, name: String, descriptor: String): Unit =
      fields += FieldAccess(
        owner,
        name,
        descriptor,
        static = opcode == Opcodes.GETSTATIC || opcode == Opcodes.PUTSTATIC,
        write = opcode == Opcodes.PUTFIELD || opcode == Opcodes.PUTSTATIC
      ): Unit

    override def visitInvokeDynamicInsn(
        name: String,
        descriptor: String,
        bootstrap: Handle,
        arguments: AnyRef*
    ): Unit =
      lambdaSite(bootstrap, arguments) match {
        case Some((sam, implementation)) =>
          val (markers, bridges) = extras(arguments.toVector, bootstrap.getName)
          lambdas += Lambda(
            Type.getReturnType(descriptor).getInternalName,
            name,
            sam.getDescriptor,
            markers,
            bridges,
            implementation.getOwner,
            implementation.getName,
            implementation.getDesc,
            implementation.getTag
          ): Unit
        case None => dynamics += Dynamic(bootstrap.getOwner, bootstrap.getName): Unit
      }

    override def visitEnd(): Unit =
      sink += Method(
        name,
        descriptor,
        access,
        invocations.toVector,
        instantiated.toSet,
        lambdas.toVector,
        dynamics.toVector,
        fields.toVector
      ): Unit
  }
}
