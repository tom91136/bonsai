package net.kurobako.bonsai.analysis

import net.kurobako.bonsai.analysis.CallGraph.*
import net.kurobako.bonsai.analysis.ClassFiles.*
import org.objectweb.asm.Opcodes
import org.objectweb.asm.Type
import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.tree.FieldInsnNode
import org.objectweb.asm.tree.InvokeDynamicInsnNode
import org.objectweb.asm.tree.LdcInsnNode
import org.objectweb.asm.tree.MethodInsnNode
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.tree.TypeInsnNode
import org.objectweb.asm.tree.analysis.Analyzer
import org.objectweb.asm.tree.analysis.AnalyzerException
import org.objectweb.asm.tree.analysis.BasicInterpreter
import org.objectweb.asm.tree.analysis.BasicValue
import org.objectweb.asm.tree.analysis.Interpreter
import org.objectweb.asm.tree.analysis.Value
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

@SuppressWarnings(Array("bonsai:test-only", "bonsai:test-only-member"))
private[analysis] object ReceiverFlow {

  /** The classes and lambdas a value can be, and whether it can also be something not named: a field, a call result, or
    * an argument no caller in the program passes.
    */
  case class Supply(receivers: Set[Receiver], open: Boolean)

  /** The receivers of the calls of one callee in one method. `own` is what the method creates or reads itself, and
    * `byCaller` holds, for each method that calls it, `own` and what that caller passes in as the receiver; it is empty
    * when the receiver does not come from an argument.
    */
  case class Supplied(own: Supply, byCaller: Map[Reference, Supply])

  private enum Source {
    case Made(name: String)
    case Closure(position: Int)
    case Argument(index: Int)
    case Unknown
  }

  private val unknown = Set[Source](Source.Unknown)
  private val nothing = Set.empty[Source]

  private case class Slot(size: Int, sources: Set[Source]) extends Value {
    override def getSize: Int = size
  }

  private case class Call(invocation: Invocation, operands: Vector[Set[Source]])

  // scalafix:off DisableSyntax.null
  private class Tracking(owner: String, node: MethodNode) extends Interpreter[Slot](Opcodes.ASM9) {
    private val basic   = BasicInterpreter()
    private val lambdas = node.instructions.asScala.view.collect { case indy: InvokeDynamicInsnNode =>
      indy
    }.filter(indy => lambdaSite(indy.bsm, indy.bsmArgs.toSeq).isDefined).zipWithIndex.toMap

    private val operands: Map[Int, Int] = {
      val types  = Type.getArgumentTypes(node.desc).toVector
      val shared = if ((node.access & Opcodes.ACC_STATIC) == 0) Vector(Type.getObjectType(owner)) else Vector.empty
      (shared ++ types).map(_.getSize).scanLeft(0)(_ + _).init.zipWithIndex.toMap
    }

    private def lift(value: BasicValue, sources: => Set[Source]): Slot =
      if (value == null) null
      else Slot(value.getSize, if (value.isReference) sources else nothing)

    override def newValue(tpe: Type): Slot = lift(basic.newValue(tpe), unknown)

    override def newParameterValue(instance: Boolean, local: Int, tpe: Type): Slot =
      lift(basic.newValue(tpe), operands.get(local).fold(unknown)(index => Set(Source.Argument(index))))

    override def newOperation(insn: AbstractInsnNode): Slot =
      lift(
        basic.newOperation(insn),
        insn match {
          case created: TypeInsnNode if insn.getOpcode == Opcodes.NEW => Set(Source.Made(created.desc))
          case constant: LdcInsnNode                                  =>
            constant.cst match {
              case _: String => Set(Source.Made("java/lang/String"))
              case _: Type   => Set(Source.Made("java/lang/Class"))
              case _         => unknown
            }
          case field: FieldInsnNode if field.name == "MODULE$" && field.desc == s"L${field.owner};" =>
            Set(Source.Made(field.owner))
          case _ if insn.getOpcode == Opcodes.ACONST_NULL => nothing
          case _                                          => unknown
        }
      )

    override def copyOperation(insn: AbstractInsnNode, value: Slot): Slot = value

    override def unaryOperation(insn: AbstractInsnNode, value: Slot): Slot =
      lift(basic.unaryOperation(insn, null), if (insn.getOpcode == Opcodes.CHECKCAST) value.sources else unknown)

    override def binaryOperation(insn: AbstractInsnNode, left: Slot, right: Slot): Slot =
      lift(basic.binaryOperation(insn, null, null), unknown)

    override def ternaryOperation(insn: AbstractInsnNode, a: Slot, b: Slot, c: Slot): Slot = null

    override def naryOperation(insn: AbstractInsnNode, values: java.util.List[? <: Slot]): Slot =
      lift(
        basic.naryOperation(insn, null),
        insn match {
          case indy: InvokeDynamicInsnNode => lambdas.get(indy).fold(unknown)(position => Set(Source.Closure(position)))
          case _                           => unknown
        }
      )

    override def returnOperation(insn: AbstractInsnNode, value: Slot, expected: Slot): Unit = ()

    override def merge(left: Slot, right: Slot): Slot =
      if (left == right) left else Slot(left.size, left.sources ++ right.sources)
  }
  // scalafix:on DisableSyntax.null

  private def invocationOf(call: MethodInsnNode): Invocation =
    Invocation(kindOf(call.getOpcode), call.owner, call.name, call.desc)

  private def width(call: MethodInsnNode): Int =
    Type.getArgumentTypes(call.desc).length + (if (call.getOpcode == Opcodes.INVOKESTATIC) 0 else 1)

  private def calls(owner: String, node: MethodNode): Vector[Call] = {
    val code = node.instructions
    try {
      val frames = Analyzer[Slot](Tracking(owner, node)).analyze(owner, node)
      Vector.tabulate(code.size)(index => (code.get(index), frames(index))).collect {
        case (call: MethodInsnNode, frame) if java.util.Objects.nonNull(frame) =>
          val count = width(call)
          Call(
            invocationOf(call),
            Vector.tabulate(count)(operand => frame.getStack(frame.getStackSize - count + operand).sources)
          )
      }
    } catch {
      case _: AnalyzerException =>
        code.asScala.view.collect { case call: MethodInsnNode =>
          Call(invocationOf(call), Vector.fill(width(call))(unknown))
        }.toVector
    }
  }
}

/** Which classes and lambdas reach a virtual call, told apart by the method that passes them in. The call graph
  * resolves every call of a callee to the same receivers, so a body shared by callers with different receivers looks as
  * polluted as one that sees them all. Here the receiver of a call is followed in the bytecode of its method to where
  * it is made (`new`, a `MODULE$` load, a lambda), or to the parameter it is, and from there to the matching argument
  * of each caller, as far back as the callers go. A field, a call result or an element is open.
  *
  * Nothing is computed until asked, and only the methods on the way back are read.
  */
@SuppressWarnings(Array("bonsai:test-only", "bonsai:test-only-member"))
private[analysis] class ReceiverFlow(world: ClassWorld, graph: CallGraph) {
  import ReceiverFlow.*

  private val analysed = mutable.Map.empty[Reference, Vector[Call]]
  private val into     = mutable.Map.empty[Reference, Vector[Edge]]

  private lazy val callingMethods: Map[Reference, Vector[Reference]] = {
    val found = mutable.Map.empty[Reference, mutable.ArrayBuffer[Reference]]
    graph.reachable.foreach { caller =>
      world.method(caller).foreach(_.invocations.foreach { invocation =>
        targets(invocation).foreach(target => found.getOrElseUpdate(target, mutable.ArrayBuffer.empty) += caller: Unit)
      })
    }
    found.view.mapValues(_.distinct.toVector).toMap
  }

  private lazy val implementations: Map[Reference, Vector[Lambda]] =
    graph.lambdas.values.toVector.groupBy(lambda => Reference(lambda.owner, lambda.name, lambda.descriptor))

  private def called(invocation: Invocation): Reference =
    Reference(invocation.owner, invocation.name, invocation.descriptor)

  private def targets(invocation: Invocation): Vector[Reference] = {
    val callee = called(invocation)
    invocation.kind match {
      case Kind.Virtual | Kind.Interface if !invocation.owner.startsWith("[") =>
        graph.dispatch.getOrElse(callee, Set.empty).toVector
      case _ =>
        world.resolve(callee).map(target =>
          Reference(target.owner, target.method.name, target.method.descriptor)
        ).toVector
    }
  }

  private def callsOf(method: Reference): Vector[Call] = analysed.getOrElseUpdate(
    method,
    world.body(method).fold(Vector.empty)(calls(method.owner, _))
  )

  /** A call into `target`, with the operands it passes. */
  private case class Edge(caller: Reference, call: Call, target: Reference) {

    /** The sources of parameter `index` of the target at this call, or none when they cannot be told. */
    def passed(index: Int): Option[Set[Source]] = call.invocation.kind match {
      case Kind.Static | Kind.Special                   => call.operands.lift(index)
      case _ if world.method(target).exists(_.isStatic) =>
        val captured = Type.getArgumentTypes(target.descriptor).length -
          Type.getArgumentTypes(call.invocation.descriptor).length
        if (index < captured) None else call.operands.lift(index - captured + 1)
      case _ if call.invocation.descriptor == target.descriptor => call.operands.lift(index).map(dispatched(index, _))
      case _                                                    => None
    }

    private def dispatched(index: Int, sources: Set[Source]): Set[Source] =
      if (index != 0) sources
      else
        sources.filter {
          case Source.Made(name) =>
            world.select(name, called(call.invocation)).exists(selected =>
              Reference(selected.owner, selected.method.name, selected.method.descriptor) == target
            )
          case _ => true
        }
  }

  private def edges(target: Reference): Vector[Edge] = into.getOrElseUpdate(
    target,
    callingMethods.getOrElse(target, Vector.empty).flatMap(caller =>
      callsOf(caller).filter(call => targets(call.invocation).contains(target)).map(Edge(caller, _, target))
    )
  )

  private def entered(method: Reference): Boolean =
    edges(method).isEmpty || implementations.getOrElse(method, Vector.empty).exists(_.tag != Opcodes.H_INVOKESTATIC)

  /** The methods that call `method`, directly or through a call that dispatches to it. */
  def callers(method: Reference): Set[Reference] = edges(method).map(_.caller).toSet

  /** The methods that hold a virtual or interface call of `callee`. */
  def holders(callee: Reference): Set[Reference] =
    graph.reachable.filter(method => world.method(method).exists(_.invocations.exists(virtual(_, callee))))

  private def virtual(invocation: Invocation, callee: Reference): Boolean =
    called(invocation) == callee && (invocation.kind == Kind.Virtual || invocation.kind == Kind.Interface)

  private class Walk(var open: Boolean) { // scalafix:ok DisableSyntax.var
    private val seen    = mutable.Set.empty[(Reference, Int)]
    private val pending = mutable.Queue.empty[(Reference, Int)]
    private val found   = mutable.Set.empty[Receiver]

    def supply: Supply = Supply(found.toSet, open)

    def take(at: Reference, sources: Set[Source]): Walk = {
      sources.foreach {
        case Source.Made(name)      => found += Receiver.Instance(name)
        case Source.Closure(index)  => found += Receiver.Closure(LambdaSite(at, index))
        case Source.Argument(index) => if (seen.add((at, index))) pending.enqueue((at, index))
        case Source.Unknown         => open = true
      }
      this
    }

    def edge(edge: Edge, index: Int): Walk = edge.passed(index).fold { open = true; this }(take(edge.caller, _))

    def drain(): Walk = {
      while (pending.nonEmpty) {
        val (method, index) = pending.dequeue()
        if (entered(method)) open = true
        edges(method).foreach(edge(_, index))
      }
      this
    }
  }

  /** The receivers of `holder`'s calls of `callee`, by the callers that pass them in. */
  def supplied(holder: Reference, callee: Reference): Supplied = {
    val sources   = callsOf(holder).filter(call => virtual(call.invocation, callee)).flatMap(_.operands.headOption)
    val all       = sources.flatten.toSet
    val arguments = all.collect { case Source.Argument(index) => index }
    val made      = all.filterNot(_.isInstanceOf[Source.Argument])
    val outside   = arguments.nonEmpty && entered(holder)
    val own       = Walk(outside).take(holder, made).supply
    val byCaller  =
      if (arguments.isEmpty) Map.empty[Reference, Supply]
      else
        edges(holder).groupBy(_.caller).view.mapValues { into =>
          val walk = Walk(outside).take(holder, made)
          into.foreach(edge => arguments.foreach(walk.edge(edge, _)))
          walk.drain().supply
        }.toMap
    Supplied(own, byCaller)
  }
}
