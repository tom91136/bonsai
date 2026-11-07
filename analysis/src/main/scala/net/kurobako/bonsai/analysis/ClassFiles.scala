package net.kurobako.bonsai.analysis

import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.Handle
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type
import scala.collection.mutable
import scala.reflect.NameTransformer
import scala.runtime.BooleanRef
import scala.runtime.IntRef
import scala.runtime.ObjectRef

private[analysis] object ClassFiles {
  enum Kind {
    case Virtual, Interface, Static, Special
  }

  case class Invocation(kind: Kind, owner: String, name: String, descriptor: String, line: Int)

  case class Lambda(
      interface: String,
      sam: String,
      owner: String,
      name: String,
      descriptor: String,
      tag: Int,
      line: Int
  )

  case class Dynamic(owner: String, name: String, descriptor: String, line: Int)

  case class FieldAccess(owner: String, name: String, descriptor: String, static: Boolean, write: Boolean)

  case class Reference(owner: String, name: String, descriptor: String)

  case class Method(
      name: String,
      descriptor: String,
      access: Int,
      line: Int,
      invocations: Vector[Invocation],
      instantiated: Set[String],
      lambdas: Vector[Lambda],
      dynamics: Vector[Dynamic],
      fields: Vector[FieldAccess],
      monitors: Boolean
  ) {
    def isStatic: Boolean   = (access & Opcodes.ACC_STATIC) != 0
    def isPrivate: Boolean  = (access & Opcodes.ACC_PRIVATE) != 0
    def isAbstract: Boolean = (access & Opcodes.ACC_ABSTRACT) != 0
    def isNative: Boolean   = (access & Opcodes.ACC_NATIVE) != 0
    def isVarargs: Boolean  = (access & Opcodes.ACC_VARARGS) != 0
  }

  case class ClassFile(
      name: String,
      version: Int,
      access: Int,
      superName: Option[String],
      interfaces: Vector[String],
      methods: Vector[Method],
      sourceFile: Option[String]
  ) {
    def isInterface: Boolean = (access & Opcodes.ACC_INTERFACE) != 0
  }

  def read(bytes: Array[Byte]): Either[String, ClassFile] =
    try {
      val visitor = Visitor()
      ClassReader(bytes).accept(visitor, ClassReader.SKIP_FRAMES)
      Right(visitor.file)
    } catch {
      case error: IllegalArgumentException => Left(error.getMessage)
    }

  def key(reference: Reference): String = {
    val binary = reference.owner.replace('/', '.').replace('$', '.').stripSuffix(".")
    val simple = if (reference.name == "<init>") reference.name else NameTransformer.decode(reference.name)
    s"$binary.$simple${reference.descriptor}"
  }

  private val lambdaMetafactory = "java/lang/invoke/LambdaMetafactory"

  private def kindOf(opcode: Int): Kind = opcode match {
    case Opcodes.INVOKEVIRTUAL   => Kind.Virtual
    case Opcodes.INVOKEINTERFACE => Kind.Interface
    case Opcodes.INVOKESTATIC    => Kind.Static
    case _                       => Kind.Special
  }

  private class Visitor extends ClassVisitor(Opcodes.ASM9) {
    private val header = ObjectRef.create(
      ClassFile(
        name = "",
        version = 0,
        access = 0,
        superName = None,
        interfaces = Vector.empty,
        methods = Vector.empty,
        sourceFile = None
      )
    )
    private val methods = mutable.ArrayBuffer.empty[Method]

    override def visit(
        version: Int,
        access: Int,
        name: String,
        signature: String,
        superName: String,
        interfaces: Array[String]
    ): Unit =
      header.elem = ClassFile(name, version, access, Option(superName), interfaces.toVector, Vector.empty, None)

    override def visitSource(source: String, debug: String): Unit =
      header.elem = header.elem.copy(sourceFile = Option(source))

    override def visitMethod(
        access: Int,
        name: String,
        descriptor: String,
        signature: String,
        exceptions: Array[String]
    ): MethodVisitor =
      Body(methods, access, name, descriptor)

    def file: ClassFile = header.elem.copy(methods = methods.toVector)
  }

  private class Body(sink: mutable.ArrayBuffer[Method], access: Int, name: String, descriptor: String)
      extends MethodVisitor(Opcodes.ASM9) {
    private val invocations  = mutable.ArrayBuffer.empty[Invocation]
    private val instantiated = mutable.Set.empty[String]
    private val lambdas      = mutable.ArrayBuffer.empty[Lambda]
    private val dynamics     = mutable.ArrayBuffer.empty[Dynamic]
    private val fields       = mutable.ArrayBuffer.empty[FieldAccess]
    private val monitors     = BooleanRef.create(false)
    private val first        = IntRef.create(0)
    private val current      = IntRef.create(0)

    override def visitLineNumber(line: Int, start: Label): Unit = {
      if (first.elem == 0) first.elem = line
      current.elem = line
    }

    override def visitTypeInsn(opcode: Int, internalName: String): Unit =
      if (opcode == Opcodes.NEW) instantiated += internalName: Unit

    override def visitInsn(opcode: Int): Unit =
      if (opcode == Opcodes.MONITORENTER) monitors.elem = true

    override def visitMethodInsn(
        opcode: Int,
        owner: String,
        name: String,
        descriptor: String,
        isInterface: Boolean
    ): Unit =
      invocations += Invocation(kindOf(opcode), owner, name, descriptor, current.elem): Unit

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
      if (bootstrap.getOwner == lambdaMetafactory) {
        val implementation = arguments(1).asInstanceOf[Handle]
        lambdas += Lambda(
          Type.getReturnType(descriptor).getInternalName,
          name,
          implementation.getOwner,
          implementation.getName,
          implementation.getDesc,
          implementation.getTag,
          current.elem
        ): Unit
      } else dynamics += Dynamic(bootstrap.getOwner, bootstrap.getName, descriptor, current.elem): Unit

    override def visitEnd(): Unit =
      sink += Method(
        name,
        descriptor,
        access,
        first.elem,
        invocations.toVector,
        instantiated.toSet,
        lambdas.toVector,
        dynamics.toVector,
        fields.toVector,
        monitors.elem
      ): Unit
  }
}
