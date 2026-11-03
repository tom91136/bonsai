package net.kurobako.bonsai.effects

/** The effects of calls to code with no body to read, decided from the qualified name of the method and what is known
  * of its receiver.
  */
object Axioms {

  /** What a call is made on: whether its type derives from a class, by qualified name, and whether it is a module. */
  case class Operand(derives: String => Boolean, module: Boolean = false)

  /** A call to `name`, declared in `owner`; `generated` for a synthetic method or a default argument, `java` when
    * defined by Java code, and `returns` the qualified name of the class it returns, which is only asked for when the
    * tables need it.
    */
  class Call(
      val owner: String,
      val name: String,
      val constructor: Boolean = false,
      val receiver: Option[Operand] = None,
      val generated: Boolean = false,
      val java: Boolean = false,
      returns: => String = ""
  ) {
    val full: String        = s"$owner.$name"
    lazy val result: String = returns
  }

  val Clocks: Set[String] = Set(
    "java.lang.System.currentTimeMillis",
    "java.lang.System.nanoTime",
    "java.util.UUID.randomUUID",
    "java.lang.Math.random"
  )

  val Randoms: Vector[String] = Vector("java.util.Random", "scala.util.Random")

  /** Methods that change a collection, a buffer, a queue or an atomic they are called on. */
  val Mutators: Set[String] = Set(
    "+=",
    "++=",
    "-=",
    "--=",
    "subtractOne",
    "subtractAll",
    "update",
    "updateWith",
    "append",
    "appendAll",
    "prepend",
    "prependAll",
    "insert",
    "insertAll",
    "getOrElseUpdate",
    "sortInPlaceBy",
    "sortInPlaceWith",
    "trimStart",
    "trimEnd",
    "reverse",
    "delete",
    "deleteCharAt",
    "replace",
    "lazySet",
    "offer",
    "poll",
    "push",
    "pop",
    "enqueue",
    "dequeue",
    "next",
    "compareAndSet",
    "getAndSet",
    "getAndIncrement",
    "getAndDecrement",
    "getAndAdd",
    "incrementAndGet",
    "decrementAndGet",
    "addAndGet",
    "updateAndGet",
    "getAndUpdate"
  )

  def mutates(name: String): Boolean =
    Mutators(name) || name.endsWith("InPlace") || name.endsWith("=") && !Set("==", "!=", "<=", ">=")(name) ||
      Vector("set", "add", "remove", "put", "clear").exists(name.startsWith(_))

  private val Atomics = "java.util.concurrent.atomic."

  private val Updates = Vector(
    "set",
    "lazySet",
    "compare",
    "weakCompare",
    "getAnd",
    "increment",
    "decrement",
    "add",
    "update",
    "accumulate",
    "reset"
  )

  /** Whether `name` writes an atomic, beside the mutators every receiver has. */
  private def updates(name: String): Boolean = Updates.exists(name.startsWith(_))

  // Owners whose methods act on the world, or read it, beside pure path arithmetic.
  private val Acting =
    Vector(
      "java.net.",
      "java.lang.Process",
      "java.lang.Thread",
      "java.lang.Runtime",
      "scala.sys.process.",
      "javax.swing.",
      "java.awt."
    )
  // Classes of java.net that build and take apart values, and touch no network.
  private val Values  = Set("java.net.URLDecoder", "java.net.URLEncoder", "java.net.URI")
  private val Logging = Vector("org.slf4j.", "java.util.logging.", "scala.Console")
  private val Reading = Vector("java.io.", "java.nio.", "scala.io.")
  private val Paths   =
    Vector("java.nio.file.Path", "java.nio.file.Paths", "java.nio.file.FileSystem", "java.nio.file.PathMatcher")
  private val Changing = Vector(
    "write",
    "delete",
    "create",
    "copy",
    "move",
    "set",
    "mkdir",
    "renameTo",
    "newOutputStream",
    "newBufferedWriter"
  )
  private val Naming =
    Set("getName", "getPath", "getParent", "getParentFile", "toPath", "toString", "compareTo", "equals", "hashCode")
  private val Printing = Set("println", "print", "printf")
  // Calls that only ask about a thread or process, beside those that act on it.
  private val Queries = Vector("get", "is", "has", "current", "exitValue", "pid", "info", "available")
  private val Actions = Vector(
    "submit",
    "execute",
    "invoke",
    "close",
    "shutdown",
    "start",
    "stop",
    "complete",
    "cancel",
    "lock",
    "unlock",
    "acquire",
    "release",
    "await",
    "signal",
    "interrupt",
    "flush",
    "reset",
    "write"
  )
  private val Variables = Set("getenv", "getProperty", "getProperties")
  // Methods of immutable values that write into an array or buffer they are given.
  private val Filling = Set("copyToArray", "copyToBuffer")

  val Assertion = "org.scalatest.compatible.Assertion"

  // Owners whose methods are pure apart from the functions passed to them, beside the members of the scala package
  // itself; the receiver decides for the generic collection traits, which mutable collections share.
  val Pure: Vector[String] = Vector(
    "scala.collection.",
    "scala.StringContext",
    "scala.util.boundary",
    "scala.Option",
    "scala.Some",
    "scala.None",
    "scala.util.Either",
    "scala.util.Left",
    "scala.util.Right",
    "scala.util.Try",
    "scala.util.Success",
    "scala.util.Failure",
    "scala.Tuple",
    "scala.Product",
    "scala.math.",
    "scala.runtime.",
    "scala.Predef",
    "scala.Int",
    "scala.Long",
    "scala.Short",
    "scala.Byte",
    "scala.Char",
    "scala.Double",
    "scala.Float",
    "scala.Boolean",
    "scala.Any",
    "scala.Equals",
    "scala.Function",
    "scala.PartialFunction",
    "scala.reflect.",
    "scala.deriving.",
    "scala.compiletime.",
    "scala.util.chaining.",
    "scala.util.hashing.",
    "scala.package",
    "java.lang.Object",
    "java.lang.String",
    "java.lang.Math",
    "java.lang.StrictMath",
    "java.lang.Integer",
    "java.lang.Long",
    "java.lang.Double",
    "java.lang.Float",
    "java.lang.Short",
    "java.lang.Byte",
    "java.lang.Character",
    "java.lang.Boolean",
    "java.lang.Number",
    "java.lang.Enum",
    "java.lang.CharSequence",
    "java.lang.Comparable",
    "java.math.",
    "java.util.Objects",
    "java.util.Optional",
    "scala.quoted.",
    "scala.util.matching.",
    "java.time.Duration",
    "java.util.concurrent.TimeUnit",
    // memoises in a field, which no caller observes
    "cats.Eval"
  )

  /** Classes whose instances change as they are used, which the receiver decides for. */
  private val Stateful: Vector[String] = Vector(
    "scala.collection.mutable.Iterable",
    "scala.collection.mutable.Builder",
    "scala.collection.Iterator",
    "scala.collection.ArrayOps",
    "java.util.Iterator",
    "java.lang.Iterable",
    "java.util.Map",
    "java.lang.AbstractStringBuilder",
    "java.util.concurrent.atomic.AtomicBoolean",
    "java.util.concurrent.atomic.AtomicInteger",
    "java.util.concurrent.atomic.AtomicLong",
    "java.util.concurrent.atomic.AtomicReference",
    "scala.Array"
  )

  /** Classes that write to a stream. */
  private val Outputs: Vector[String] = Vector("java.io.OutputStream", "java.io.Writer")

  /** The effects of `call`; those of a constructor are in its body. */
  def effects(call: Call): Set[Effect[Nothing]] = {
    val (owner, name, full)              = (call.owner, call.name, call.full)
    val received                         = call.receiver
    def derives(classes: Vector[String]) = received.exists(operand => classes.exists(operand.derives))
    def effect(kind: Kind)               = Set(Effect(kind, s"calls $full"))
    def stateful(kind: Kind)             = Set(Effect(kind, s"calls $full", onThis = true))
    val none                             = Set.empty[Effect[Nothing]]
    if (call.constructor) none
    else if (Clocks(full) || derives(Randoms) || owner == "scala.util.Random") effect(Kind.Random)
    else if (derives(Outputs) || Logging.exists(owner.startsWith(_)) || owner == "scala.Predef" && Printing(name))
      effect(Kind.Output)
    else if (Values(owner)) none
    else if (Acting.exists(owner.startsWith(_)))
      effect(if (Queries.exists(name.startsWith(_))) Kind.Input else Kind.Output)
    else if (full == "scala.Array.copy" || full == "java.lang.System.arraycopy") effect(Kind.Writes)
    else if (owner == "java.lang.System")
      if (Variables(name)) effect(Kind.Input)
      else if (Set("lineSeparator", "identityHashCode")(name)) none
      else effect(Kind.Output)
    else if (Paths.exists(owner.startsWith(_)) || owner == "java.io.File" && Naming(name))
      if (Set("toRealPath", "toAbsolutePath")(name)) effect(Kind.Input) else none
    else if (owner == "java.nio.file.Files" || owner == "java.io.File")
      effect(if (Changing.exists(name.startsWith(_))) Kind.Output else Kind.Input)
    else if (Reading.exists(owner.startsWith(_))) effect(Kind.Input)
    else if (derives(Stateful) || owner.startsWith(Atomics))
      if (name == "length" && received.exists(_.derives("scala.Array"))) none
      else if (Filling(name)) effect(Kind.Writes)
      else stateful(if (mutates(name) || owner.startsWith(Atomics) && updates(name)) Kind.Writes else Kind.Reads)
    else if (
      Pure.exists(owner.startsWith(_)) || call.generated ||
      owner.startsWith("scala.") && !owner.stripPrefix("scala.").contains('.')
    ) {
      // A companion's apply builds a value, where a collection's looks one up; checks fail to validate.
      val partial = Partials.Names(name) && !(name == "apply" && received.exists(_.module)) ||
        name.startsWith("check") || name.startsWith("require")
      if (Filling(name)) effect(Kind.Writes)
      else if (partial) effect(Kind.Throws)
      else none
    } else if (owner == "java.lang.Throwable" || owner == "java.lang.Class")
      if (Queries.exists(name.startsWith(_)) || name == "toString") none
      else if (name == "printStackTrace") effect(Kind.Output)
      else stateful(Kind.Writes)
    else if (owner.startsWith("munit."))
      if (name == "fail" || name.startsWith("assert")) effect(Kind.Throws)
      else none
    // The only value of ScalaTest's assertion type is success, so a call that returns it matters by throwing.
    else if (call.result == Assertion) effect(Kind.Throws)
    // Java objects are mutable: what acts on one changes it, and anything else may read it or change it.
    else if (call.java && received.exists(!_.module))
      if (mutates(name) || Actions.exists(name.startsWith(_))) stateful(Kind.Writes)
      else Set(Effect(Kind.Reads, s"calls $full", onThis = true, assumed = true))
    else effect(Kind.Unknown)
  }

  /** The summary of `call`, which runs the function and by-name parameters at the indices `runs`. */
  def summary(call: Call, runs: Set[Int]): Summary[Nothing] =
    if (call.constructor) Summary(Nil, Set.empty) else Summary(effects(call), runs)
}
