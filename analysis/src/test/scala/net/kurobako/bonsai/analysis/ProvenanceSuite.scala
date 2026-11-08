package net.kurobako.bonsai.analysis

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes.*

class ProvenanceSuite extends munit.FunSuite {
  private val owner = "fixture/Asm"

  // scalafix:off DisableSyntax.null

  @SuppressWarnings(Array("bonsai:long-arguments", "bonsai:same-type-arguments", "bonsai:boolean-argument"))
  private class Code(writer: ClassWriter, descriptor: String) {
    private val visitor: MethodVisitor = writer.visitMethod(ACC_PUBLIC | ACC_STATIC, "m", descriptor, null, null)
    visitor.visitCode()

    def op(opcode: Int): Unit                               = visitor.visitInsn(opcode)
    def local(opcode: Int, index: Int): Unit                = visitor.visitVarInsn(opcode, index)
    def jump(opcode: Int, to: Label): Unit                  = visitor.visitJumpInsn(opcode, to)
    def at(label: Label): Unit                              = visitor.visitLabel(label)
    def make(opcode: Int, target: String): Unit             = visitor.visitTypeInsn(opcode, target)
    def newArray(): Unit                                    = visitor.visitIntInsn(NEWARRAY, T_INT)
    def decrement(index: Int): Unit                         = visitor.visitIincInsn(index, -1)
    def field(opcode: Int): Unit                            = visitor.visitFieldInsn(opcode, owner, "value", "I")
    def guard(from: Label, to: Label, handler: Label): Unit =
      visitor.visitTryCatchBlock(from, to, handler, "java/lang/RuntimeException")
    def call(opcode: Int, target: String, name: String, descriptor: String = "()V"): Unit =
      visitor.visitMethodInsn(opcode, target, name, descriptor, false)
    def finish(): Unit = {
      visitor.visitMaxs(8, 8)
      visitor.visitEnd()
    }

    def collect(): Unit = call(INVOKESTATIC, "java/lang/System", "gc")

    def raise(): Unit = {
      make(NEW, "java/lang/IllegalStateException")
      op(DUP)
      call(INVOKESPECIAL, "java/lang/IllegalStateException", "<init>")
      op(ATHROW)
    }
  }

  @SuppressWarnings(Array("bonsai:long-arguments"))
  private def trace(descriptor: String)(body: Code => Unit): Trace = {
    val writer = ClassWriter(0)
    writer.visit(V21, ACC_PUBLIC, owner, null, "java/lang/Object", null)
    val code = Code(writer, descriptor)
    body(code)
    code.finish()
    writer.visitEnd()
    val bytes = writer.toByteArray
    val found = ClassFiles.read(bytes).fold(error => fail(error), identity).methods.find(_.name == "m")
    ClassFiles.trace(bytes, owner, Vector(found.getOrElse(fail("no method m"))))(("m", descriptor))
  }

  // scalafix:on DisableSyntax.null

  test("code the analyser cannot follow is assumed to do the worst") {
    val found = trace("(Ljava/lang/Object;)V") { code =>
      code.local(ALOAD, 0)
      code.call(INVOKEVIRTUAL, "java/lang/Object", "hashCode", "()I")
      code.op(POP)
      code.local(ALOAD, 0)
      code.field(GETFIELD)
      code.op(POP)
      code.op(POP)
      code.op(RETURN)
    }
    assertEquals(
      found,
      Trace(
        Vector(Origin.Other),
        Vector(false),
        Vector(Origin.Other),
        Set(Origin.Other),
        Set(Origin.Other),
        Set(Origin.Other),
        throws = true,
        loops = true,
        allocates = true
      )
    )
  }

  test("code no path reaches does nothing") {
    val found = trace("(Ljava/lang/Object;)V") { code =>
      val end = Label()
      code.jump(GOTO, end)
      code.local(ALOAD, 0)
      code.call(INVOKEVIRTUAL, "java/lang/Object", "hashCode", "()I")
      code.op(POP)
      code.local(ALOAD, 0)
      code.field(GETFIELD)
      code.op(POP)
      code.local(ALOAD, 0)
      code.op(ICONST_0)
      code.op(ICONST_1)
      code.op(IASTORE)
      code.local(ALOAD, 0)
      code.op(ICONST_0)
      code.op(IALOAD)
      code.op(POP)
      code.local(ALOAD, 0)
      code.op(MONITORENTER)
      code.op(ICONST_1)
      code.newArray()
      code.op(POP)
      code.local(ALOAD, 0)
      code.op(ATHROW)
      code.at(end)
      code.op(RETURN)
    }
    assertEquals(
      found,
      Trace(
        Vector(Origin.Other),
        Vector(false),
        Vector(Origin.Other),
        Set.empty,
        Set.empty,
        Set.empty,
        throws = false,
        loops = false,
        allocates = false
      )
    )
  }

  test("a handler is followed from the code it guards") {
    val found = trace("()V") { code =>
      val (start, end, handler, done) = (Label(), Label(), Label(), Label())
      code.guard(start, end, handler)
      code.at(start)
      code.collect()
      code.at(end)
      code.jump(GOTO, done)
      code.at(handler)
      code.op(POP)
      code.make(NEW, owner)
      code.op(DUP)
      code.call(INVOKESPECIAL, owner, "<init>")
      code.local(ASTORE, 0)
      code.local(ALOAD, 0)
      code.op(ICONST_1)
      code.field(PUTFIELD)
      code.at(done)
      code.op(RETURN)
    }
    assertEquals(found.receivers, Vector(Origin.Other, Origin.Fresh))
    assertEquals(found.holders, Vector(Origin.Fresh))
  }

  test("a call that can return is not one that only leads to a throw, whichever way the method returns") {
    def leading(returned: Int, descriptor: String) = trace(descriptor) { code =>
      val ok = Label()
      code.collect()
      code.local(ILOAD, 0)
      code.jump(IFEQ, ok)
      code.raise()
      code.at(ok)
      if (returned == IRETURN) code.op(ICONST_1)
      code.op(returned)
    }
    assertEquals(leading(IRETURN, "(I)I").exceptional, Vector(false, true))
    assertEquals(leading(RETURN, "(I)V").exceptional, Vector(false, true))
  }

  test("a loop is a jump back to an instruction that was reached, to itself included") {
    val spinning = trace("()V") { code =>
      val again = Label()
      code.at(again)
      code.jump(GOTO, again)
    }
    assert(spinning.loops)
    val calling = trace("()V") { code =>
      val again = Label()
      code.at(again)
      code.collect()
      code.jump(GOTO, again)
    }
    assert(calling.loops)
    assertEquals(calling.exceptional, Vector(false))
  }

  test("a loop or an array before a throw that may not happen still counts") {
    val looping = trace("(II)V") { code =>
      val (head, body, after, ok) = (Label(), Label(), Label(), Label())
      code.at(head)
      code.local(ILOAD, 0)
      code.jump(IFNE, body)
      code.jump(GOTO, after)
      code.at(body)
      code.decrement(0)
      code.jump(GOTO, head)
      code.at(after)
      code.local(ILOAD, 1)
      code.jump(IFEQ, ok)
      code.raise()
      code.at(ok)
      code.op(RETURN)
    }
    assert(looping.loops)
    val allocating = trace("(I)V") { code =>
      val ok = Label()
      code.op(ICONST_1)
      code.newArray()
      code.op(POP)
      code.local(ILOAD, 0)
      code.jump(IFEQ, ok)
      code.raise()
      code.at(ok)
      code.op(RETURN)
    }
    assert(allocating.allocates)
  }

  test("an array only the path to a throw allocates is no allocation") {
    val found = trace("(I)V") { code =>
      val ok = Label()
      code.local(ILOAD, 0)
      code.jump(IFEQ, ok)
      code.op(ICONST_1)
      code.newArray()
      code.op(POP)
      code.raise()
      code.at(ok)
      code.op(RETURN)
    }
    assert(!found.allocates)
    assert(found.throws)
  }
}
