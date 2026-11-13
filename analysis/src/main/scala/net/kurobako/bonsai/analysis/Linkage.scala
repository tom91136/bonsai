package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.util.jar.JarFile
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.Handle
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type
import org.objectweb.asm.signature.SignatureReader
import org.objectweb.asm.signature.SignatureVisitor
import scala.collection.mutable
import scala.util.Try
import scala.util.Using

/** What the classes of a compile link against: the classes and members their bytecode names, and whether another
  * version of a library resolves them as the version the compile was built against does.
  */
private[analysis] object Linkage {

  object References {
    val empty: References = References(Set.empty, Set.empty)
  }

  /** The classes, by internal name, and the members, each its owner and its name with its descriptor, that classes
    * name.
    */
  case class References(classes: Set[String], members: Set[(String, String)]) {
    def merged(other: References): References = References(classes ++ other.classes, members ++ other.members)
  }

  private class Collected {
    val classes: mutable.Set[String]           = mutable.Set.empty
    val members: mutable.Set[(String, String)] = mutable.Set.empty

    def named(name: String | Null): Unit = Option(name).foreach(found => typed(Type.getObjectType(found)))

    def typed(value: Type): Unit = value.getSort match {
      case Type.OBJECT => classes += value.getInternalName: Unit
      case Type.ARRAY  => typed(value.getElementType)
      case Type.METHOD =>
        value.getArgumentTypes.foreach(typed(_))
        typed(value.getReturnType)
      case _ => ()
    }

    def described(descriptor: String): Unit = typed(Type.getType(descriptor))

    def signed(signature: String | Null): Unit =
      Option(signature).foreach(found => SignatureReader(found).accept(Naming(this)))

    def member(owner: String, name: String, descriptor: String): Unit = {
      named(owner)
      described(descriptor)
      if (!owner.startsWith("[")) members += owner -> key(name, descriptor): Unit
    }

    def constant(value: Any): Unit = value match {
      case value: Type   => typed(value)
      case value: Handle => member(value.getOwner, value.getName, value.getDesc)
      case _             => ()
    }
  }

  private class Naming(into: Collected) extends SignatureVisitor(Opcodes.ASM9) {
    override def visitClassType(name: String): Unit = into.named(name)
  }

  private class Coding(into: Collected) extends MethodVisitor(Opcodes.ASM9) {
    override def visitTypeInsn(opcode: Int, name: String): Unit = into.named(name)

    override def visitFieldInsn(opcode: Int, owner: String, name: String, descriptor: String): Unit =
      into.member(owner, name, descriptor)

    override def visitMethodInsn(opcode: Int, owner: String, name: String, descriptor: String, interface: Boolean)
        : Unit = into.member(owner, name, descriptor)

    override def visitInvokeDynamicInsn(name: String, descriptor: String, bootstrap: Handle, arguments: Object*)
        : Unit = {
      into.described(descriptor)
      into.constant(bootstrap)
      arguments.foreach(into.constant(_))
    }

    override def visitLdcInsn(value: Any): Unit = into.constant(value)

    override def visitMultiANewArrayInsn(descriptor: String, dimensions: Int): Unit = into.described(descriptor)

    override def visitTryCatchBlock(start: Label, end: Label, handler: Label, kind: String): Unit = into.named(kind)
  }

  private class Referencing(into: Collected) extends ClassVisitor(Opcodes.ASM9) {
    private val code = Coding(into)

    override def visit(
        version: Int,
        access: Int,
        name: String,
        signature: String,
        superName: String,
        interfaces: Array[String]
    ): Unit = {
      into.named(superName)
      interfaces.foreach(into.named(_))
      into.signed(signature)
    }

    override def visitField(access: Int, name: String, descriptor: String, signature: String, value: Any)
        : FieldVisitor = {
      into.described(descriptor)
      into.signed(signature)
      null // scalafix:ok DisableSyntax.null
    }

    override def visitMethod(
        access: Int,
        name: String,
        descriptor: String,
        signature: String,
        exceptions: Array[String]
    ): MethodVisitor = {
      into.described(descriptor)
      into.signed(signature)
      Option(exceptions).foreach(_.foreach(into.named(_)))
      code
    }
  }

  /** The references of the class files among `files`. */
  def of(files: Vector[Path]): References = {
    val collected = Collected()
    val visitor   = Referencing(collected)
    files.filter(_.toString.endsWith(".class")).foreach(file =>
      ClassReader(Files.readAllBytes(file)).accept(visitor, ClassReader.SKIP_FRAMES | ClassReader.SKIP_DEBUG)
    )
    References(collected.classes.toSet, collected.members.toSet)
  }

  private def key(name: String, descriptor: String): String =
    if (descriptor.startsWith("(")) name + descriptor else s"$name:$descriptor"

  private case class Header(parents: Vector[String], members: Set[String])

  private class Declaring extends ClassVisitor(Opcodes.ASM9) {
    val parents: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty
    val members: mutable.Set[String]         = mutable.Set.empty

    override def visit(
        version: Int,
        access: Int,
        name: String,
        signature: String,
        superName: String,
        interfaces: Array[String]
    ): Unit = parents ++= Option(superName) ++ interfaces: Unit

    override def visitField(access: Int, name: String, descriptor: String, signature: String, value: Any)
        : FieldVisitor = {
      if ((access & Opcodes.ACC_PRIVATE) == 0) members += key(name, descriptor)
      null // scalafix:ok DisableSyntax.null
    }

    override def visitMethod(
        access: Int,
        name: String,
        descriptor: String,
        signature: String,
        exceptions: Array[String]
    ): MethodVisitor = {
      if ((access & Opcodes.ACC_PRIVATE) == 0) members += key(name, descriptor)
      null // scalafix:ok DisableSyntax.null
    }
  }

  private class Archive(jar: JarFile) {
    private val headers = mutable.Map.empty[String, Option[Header]]

    def has(name: String): Boolean = Option(jar.getEntry(s"$name.class")).nonEmpty

    private def header(name: String): Option[Header] = headers.getOrElseUpdate(
      name,
      Option(jar.getEntry(s"$name.class")).flatMap(entry =>
        Try {
          val declaring = Declaring()
          ClassReader(Using.resource(jar.getInputStream(entry))(_.readAllBytes())).accept(
            declaring,
            ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES
          )
          Header(declaring.parents.toVector, declaring.members.toSet)
        }.toOption
      )
    )

    /** Where `member` of `owner` resolves: the class of the library that declares it, else the classes beyond the
      * library that `owner` inherits from, where the search leaves it.
      */
    def resolution(owner: String, member: String): Either[Set[String], String] = {
      val seen                      = mutable.LinkedHashSet.empty[String]
      val outside                   = mutable.Set.empty[String]
      def climb(name: String): Unit = if (seen.add(name)) header(name) match {
        case None         => outside += name: Unit
        case Some(header) => header.parents.foreach(climb(_))
      }
      climb(owner)
      seen.find(name => header(name).exists(_.members(member))).toRight(outside.toSet)
    }
  }

  /** A reference among `references` that the library `newer` resolves otherwise than `older`, which the classes were
    * compiled against: a class of `older` that `newer` lacks, or a member of a class of `older` that `newer` declares
    * in another class or lacks.
    */
  def lost(references: References, older: Path, newer: Path): Option[String] =
    Using.resources(JarFile(older.toFile), JarFile(newer.toFile)) { (olderJar, newerJar) =>
      val before = Archive(olderJar)
      val after  = Archive(newerJar)
      references.classes.find(name => before.has(name) && !after.has(name)).map(_.replace('/', '.')).orElse(
        references.members.collectFirst {
          case (owner, member)
              if before.has(owner) && before.resolution(owner, member) != after.resolution(owner, member) =>
            s"${owner.replace('/', '.')}.${member.takeWhile(character => character != '(' && character != ':')}"
        }
      )
    }
}
