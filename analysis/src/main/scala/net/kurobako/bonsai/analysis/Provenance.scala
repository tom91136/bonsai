package net.kurobako.bonsai.analysis

import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type
import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.tree.FieldInsnNode
import org.objectweb.asm.tree.InvokeDynamicInsnNode
import org.objectweb.asm.tree.LdcInsnNode
import org.objectweb.asm.tree.MethodInsnNode
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.tree.TryCatchBlockNode
import org.objectweb.asm.tree.analysis.Analyzer
import org.objectweb.asm.tree.analysis.AnalyzerException
import org.objectweb.asm.tree.analysis.Frame
import org.objectweb.asm.tree.analysis.Interpreter
import org.objectweb.asm.tree.analysis.Value
import scala.collection.mutable

private[analysis] object Provenance {
  val none: Trace =
    Trace(Vector.empty, Vector.empty, Vector.empty, Set.empty, Set.empty, Set.empty, false, false, false)

  private def unknown(invocations: Int, fields: Int): Trace = Trace(
    Vector.fill(invocations)(Origin.Other),
    Vector.fill(invocations)(false),
    Vector.fill(fields)(Origin.Other),
    Set(Origin.Other),
    Set(Origin.Other),
    Set(Origin.Other),
    throws = true,
    loops = true,
    allocates = true
  )

  /** The trace of `node`, a method of the class `owner` with `invocations` method instructions and `fields` field
    * instructions; the worst case for code the analyser cannot follow.
    */
  def of(owner: String, node: MethodNode, invocations: Int, fields: Int): Trace =
    try follow(owner, node)
    catch { case _: AnalyzerException => unknown(invocations, fields) }

  private class Slot(val size: Int, val origin: Origin) extends Value {
    override def getSize: Int = size
  }

  private val slots =
    Array.tabulate(2, Origin.values.length)((size, origin) => Slot(size + 1, Origin.fromOrdinal(origin)))

  private def slot(size: Int, origin: Origin = Origin.Other): Slot = slots(size - 1)(origin.ordinal)

  private def sized(tpe: Type): Slot = slot(tpe.getSize)

  // scalafix:off DisableSyntax.null
  // A static field of its own class that an initialiser reads is the object it is building.
  private class Interpretation(owner: String, initialising: Boolean) extends Interpreter[Slot](Opcodes.ASM9) {
    override def newValue(tpe: Type): Slot = Option(tpe).fold(slot(1))(returned(_))

    override def newExceptionValue(block: TryCatchBlockNode, frame: Frame[Slot], tpe: Type): Slot =
      slot(1, Origin.Caught)

    override def newParameterValue(instance: Boolean, local: Int, tpe: Type): Slot =
      if (instance && local == 0) slot(1, Origin.This) else newValue(tpe)

    override def newOperation(insn: AbstractInsnNode): Slot = insn match {
      case constant: LdcInsnNode =>
        (constant.cst: Any) match {
          case _: Long | _: Double => slot(2)
          case _                   => slot(1)
        }
      case field: FieldInsnNode =>
        val held = Type.getType(field.desc)
        if (initialising && field.owner == owner && held.getSize == 1) slot(1, Origin.Fresh) else sized(held)
      case _ =>
        insn.getOpcode match {
          case Opcodes.NEW                                                               => slot(1, Origin.Fresh)
          case Opcodes.LCONST_0 | Opcodes.LCONST_1 | Opcodes.DCONST_0 | Opcodes.DCONST_1 => slot(2)
          case _                                                                         => slot(1)
        }
    }

    override def copyOperation(insn: AbstractInsnNode, value: Slot): Slot = value

    override def unaryOperation(insn: AbstractInsnNode, value: Slot): Slot = insn match {
      case field: FieldInsnNode => if (insn.getOpcode == Opcodes.GETFIELD) sized(Type.getType(field.desc)) else null
      case _                    =>
        insn.getOpcode match {
          case Opcodes.LNEG | Opcodes.DNEG | Opcodes.I2L | Opcodes.I2D | Opcodes.L2D | Opcodes.F2L | Opcodes.F2D |
              Opcodes.D2L => slot(2)
          case Opcodes.NEWARRAY | Opcodes.ANEWARRAY => slot(1, Origin.Fresh)
          case Opcodes.CHECKCAST                    => value
          case Opcodes.INEG | Opcodes.FNEG | Opcodes.L2I | Opcodes.L2F | Opcodes.F2I | Opcodes.D2I | Opcodes.D2F |
              Opcodes.I2F | Opcodes.I2B | Opcodes.I2C | Opcodes.I2S | Opcodes.ARRAYLENGTH | Opcodes.INSTANCEOF |
              Opcodes.IINC =>
            slot(1)
          case _ => null
        }
    }

    override def binaryOperation(insn: AbstractInsnNode, left: Slot, right: Slot): Slot = insn.getOpcode match {
      case Opcodes.LALOAD | Opcodes.DALOAD | Opcodes.LADD | Opcodes.LSUB | Opcodes.LMUL | Opcodes.LDIV |
          Opcodes.LREM | Opcodes.LSHL | Opcodes.LSHR | Opcodes.LUSHR | Opcodes.LAND | Opcodes.LOR | Opcodes.LXOR |
          Opcodes.DADD | Opcodes.DSUB | Opcodes.DMUL | Opcodes.DDIV | Opcodes.DREM => slot(2)
      case Opcodes.IF_ICMPEQ | Opcodes.IF_ICMPNE | Opcodes.IF_ICMPLT | Opcodes.IF_ICMPGE | Opcodes.IF_ICMPGT |
          Opcodes.IF_ICMPLE | Opcodes.IF_ACMPEQ | Opcodes.IF_ACMPNE | Opcodes.PUTFIELD => null
      case _ => slot(1)
    }

    override def ternaryOperation(insn: AbstractInsnNode, a: Slot, b: Slot, c: Slot): Slot = null

    override def naryOperation(insn: AbstractInsnNode, values: java.util.List[? <: Slot]): Slot = insn match {
      case call: MethodInsnNode        => returned(Type.getReturnType(call.desc))
      case call: InvokeDynamicInsnNode => returned(Type.getReturnType(call.desc))
      case _                           => slot(1, Origin.Fresh)
    }

    private def returned(tpe: Type): Slot = if (tpe.getSort == Type.VOID) null else sized(tpe)

    override def returnOperation(insn: AbstractInsnNode, value: Slot, expected: Slot): Unit = ()

    override def merge(left: Slot, right: Slot): Slot =
      if (left eq right) left else if (left.size == right.size) slot(left.size) else slot(1)
  }
  // scalafix:on DisableSyntax.null

  private class Edges(interpretation: Interpretation) extends Analyzer[Slot](interpretation) {
    val normalFrom: mutable.ArrayBuilder.ofInt = mutable.ArrayBuilder.ofInt()
    val normalTo: mutable.ArrayBuilder.ofInt   = mutable.ArrayBuilder.ofInt()
    val thrownFrom: mutable.ArrayBuilder.ofInt = mutable.ArrayBuilder.ofInt()
    val thrownTo: mutable.ArrayBuilder.ofInt   = mutable.ArrayBuilder.ofInt()

    override protected def newControlFlowEdge(from: Int, to: Int): Unit = {
      normalFrom += from
      normalTo += to
    }

    override protected def newControlFlowExceptionEdge(from: Int, to: Int): Boolean = {
      thrownFrom += from
      thrownTo += to
      true
    }
  }

  private val Allocations = Set(Opcodes.NEWARRAY, Opcodes.ANEWARRAY, Opcodes.MULTIANEWARRAY)

  private def isReturn(opcode: Int): Boolean = opcode >= Opcodes.IRETURN && opcode <= Opcodes.RETURN

  /** The edges of a control flow graph read backwards, by the instruction each ends at. */
  private class Predecessors(count: Int, from: Array[Int], to: Array[Int]) {
    private val starts  = Array.ofDim[Int](count + 1)
    private val sources = Array.ofDim[Int](from.length)
    to.foreach(target => starts(target + 1) += 1)
    (0 until count).foreach(index => starts(index + 1) += starts(index))
    private val filled = starts.clone()
    from.indices.foreach { edge =>
      sources(filled(to(edge))) = from(edge)
      filled(to(edge)) += 1
    }

    /** The instructions from which one of `sinks` can be reached. */
    def reaching(sinks: Iterator[Int]): java.util.BitSet = {
      val reached = java.util.BitSet(count)
      val pending = mutable.Stack.from(sinks)
      while (pending.nonEmpty) {
        val next = pending.pop()
        if (!reached.get(next)) {
          reached.set(next)
          (starts(next) until starts(next + 1)).foreach(position => pending.push(sources(position)))
        }
      }
      reached
    }
  }

  private def follow(owner: String, node: MethodNode): Trace = {
    val analyzer = Edges(Interpretation(owner, node.name == "<clinit>"))
    val frames   = analyzer.analyze(owner, node)
    val code     = node.instructions
    val count    = code.size
    val opcodes  = Array.tabulate(count)(code.get(_).getOpcode)
    val normal   = (analyzer.normalFrom.result(), analyzer.normalTo.result())
    val edges = Predecessors(count, normal._1 ++ analyzer.thrownFrom.result(), normal._2 ++ analyzer.thrownTo.result())
    val returns = edges.reaching(Iterator.range(0, count).filter(index => isReturn(opcodes(index))))
    val throws  = edges.reaching(Iterator.range(0, count).filter(opcodes(_) == Opcodes.ATHROW))
    def exceptional(index: Int): Boolean = !returns.get(index) && throws.get(index)

    def stack(index: Int, below: Int): Origin = {
      val frame = frames(index)
      frame.getStack(frame.getStackSize - 1 - below).origin
    }

    def live(index: Int): Boolean = java.util.Objects.nonNull(frames(index))

    val receivers  = Vector.newBuilder[Origin]
    val exceptions = Vector.newBuilder[Boolean]
    val holders    = Vector.newBuilder[Origin]
    val stores     = mutable.Set.empty[Origin]
    val loads      = mutable.Set.empty[Origin]
    val locked     = mutable.Set.empty[Origin]
    Iterator.range(0, count).foreach { index =>
      val insn = code.get(index)
      insn match {
        case call: MethodInsnNode =>
          val static = call.getOpcode == Opcodes.INVOKESTATIC
          receivers += (if (live(index) && !static) stack(index, Type.getArgumentCount(call.desc)) else Origin.Other)
          exceptions += (live(index) && exceptional(index))
        case access: FieldInsnNode =>
          val depth = access.getOpcode match {
            case Opcodes.PUTFIELD => Some(1)
            case Opcodes.GETFIELD => Some(0)
            case _                => None
          }
          holders += (if (live(index)) depth.fold(Origin.Other)(stack(index, _)) else Origin.Other)
        case _ => ()
      }
      if (live(index)) insn.getOpcode match {
        case Opcodes.IASTORE | Opcodes.LASTORE | Opcodes.FASTORE | Opcodes.DASTORE | Opcodes.AASTORE |
            Opcodes.BASTORE | Opcodes.CASTORE | Opcodes.SASTORE => stores += stack(index, 2)
        case Opcodes.IALOAD | Opcodes.LALOAD | Opcodes.FALOAD | Opcodes.DALOAD | Opcodes.AALOAD | Opcodes.BALOAD |
            Opcodes.CALOAD | Opcodes.SALOAD => loads += stack(index, 1)
        case Opcodes.MONITORENTER => locked += stack(index, 0)
        case _                    => ()
      }
    }
    val allocates = Iterator.range(0, count).exists { index =>
      live(index) && !exceptional(index) && Allocations(opcodes(index))
    }
    val throwing = Iterator.range(0, count).exists(index =>
      live(index) && opcodes(index) == Opcodes.ATHROW && stack(index, 0) != Origin.Caught
    )
    val loops = normal._1.indices.exists { edge =>
      val (from, to) = (normal._1(edge), normal._2(edge))
      to <= from && !exceptional(from) && !exceptional(to)
    }
    Trace(
      receivers.result(),
      exceptions.result(),
      holders.result(),
      stores.toSet,
      loads.toSet,
      locked.toSet,
      throwing,
      loops,
      allocates
    )
  }
}
