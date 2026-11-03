package net.kurobako.bonsai.analysis

import java.io.ByteArrayInputStream
import java.io.DataInputStream
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable

/** Method sizes read from compiled class files, against the limits HotSpot and the class file format impose. */
object Bytecode {

  /** The code length of method `name` in the class with binary name `owner`, such as `p/Outer$Inner$`. */
  case class Method(owner: String, name: String, size: Int)

  // HotSpot's HugeMethodLimit: with the default DontCompileHugeMethods, larger methods are only ever interpreted.
  private val HugeMethod = 8000
  private val Limit      = 65535
  private val NearLimit  = 0.8

  def read(directories: Vector[Path]): Vector[Method] = directories.view.filter(Files.isDirectory(_)).flatMap {
    directory =>
      Outputs.files(directory).filter(_.toString.endsWith(".class")).flatMap(file => methods(Files.readAllBytes(file)))
  }.toVector

  /** Methods near the 64 KB limit, and those HotSpot only interprets, which matter in the `hot` nodes when the build
    * marks any.
    */
  def findings(methods: Vector[Method], nodes: Map[String, Node], hot: Option[Set[String]]): Vector[Finding] = {
    // The compiler writes fromOrdinal on the companion of every enum with singleton cases, and valueOf only when none
    // of them passes arguments to the enum's constructor.
    val enumerations = methods.view.collect { case method if method.name == "fromOrdinal" => method.owner }.toSet
    methods.view.filter(method => method.size > HugeMethod && !once(method)).flatMap { method =>
      val (node, shown) = describe(method, nodes)
      val detail        = s"$shown has ${method.size} bytes of bytecode"
      val written       = enumerations(method.owner) && (method.name == "valueOf" || method.name == "fromOrdinal")
      node.flatMap { key =>
        def at(rule: Finding.Rule, text: String, level: Finding.Level = Finding.Level.Warning) =
          Finding.at(nodes, key, rule, text, level)
        if (method.size > Limit * NearLimit)
          at(Finding.Rule.MethodLimit, s"$detail, ${method.size * 100 / Limit}% of the 64 KB method limit")
        else if (written)
          at(
            Finding.Rule.HugeMethod,
            s"$detail, which the compiler writes for an enum this large; HotSpot interprets it, and only splitting the " +
              "enum shrinks it",
            Finding.Level.Info
          )
        else if (hot.forall(_(key)))
          at(
            Finding.Rule.HugeMethod,
            s"$detail; HotSpot interprets methods over $HugeMethod bytes instead of compiling them"
          )
        else
          at(
            Finding.Rule.HugeMethod,
            s"$detail; HotSpot interprets methods over $HugeMethod bytes, which costs little outside the classes the " +
              "build marks hot",
            Finding.Level.Info
          )
      }
    }.toVector
  }

  // Static and object initialisers and lazy value initialisers run once, so interpreting them costs nothing lasting.
  private def once(method: Method): Boolean =
    method.name == "<clinit>" || method.name.contains("$lzyINIT") ||
      method.name == "<init>" && method.owner.endsWith("$")

  /** The node a method belongs to and its readable name, with lambda bodies shown under their defining method. */
  private def describe(method: Method, nodes: Map[String, Node]): (Option[String], String) = {
    val binary = method.owner.replace('/', '.')
    val outer  = binary.indexOf("$$") match {
      case -1    => binary
      case index => binary.take(index)
    }
    val topLevel = outer.endsWith("$package$")
    val name     = outer.stripSuffix("$").split('$').filter(_.nonEmpty).mkString(".")
    val lambda   = method.name.contains("$anonfun$")
    val plain    = method.name.stripPrefix("$anonfun$").split('$').find(_.nonEmpty).getOrElse(method.name)
    val shown    = if (lambda) s"$plain (lambda)" else plain
    // A top-level method's node is its own name in its package, without the object named after its source file that
    // the compiler groups it into.
    val owner = if (topLevel) (name.split('.').dropRight(2) :+ plain).mkString(".") else name
    val node  =
      Iterator.iterate(owner)(_.split('.').dropRight(1).mkString(".")).takeWhile(_.nonEmpty).find(nodes.contains)
    val label = node.fold(shown)(key => if (topLevel) shown else s"${Finding.simple(key)}.$shown")
    (node, label)
  }

  /** The code length of every method in a class file. */
  private def methods(bytes: Array[Byte]): Vector[Method] = {
    val input     = DataInputStream(ByteArrayInputStream(bytes))
    val _         = (input.readInt(), input.readUnsignedShort(), input.readUnsignedShort())
    val count     = input.readUnsignedShort()
    val constants = mutable.Map.empty[Int, String]
    val classes   = mutable.Map.empty[Int, Int]
    Iterator.iterate(1)(index =>
      input.readUnsignedByte() match {
        case 1 =>
          constants(index) = input.readUTF()
          index + 1
        case 7 =>
          classes(index) = input.readUnsignedShort()
          index + 1
        case 5 | 6 =>
          input.skipNBytes(8)
          index + 2
        case 3 | 4 | 9 | 10 | 11 | 12 | 17 | 18 =>
          input.skipNBytes(4)
          index + 1
        case 15 =>
          input.skipNBytes(3)
          index + 1
        case 8 | 16 | 19 | 20 =>
          input.skipNBytes(2)
          index + 1
        case tag => throw IllegalArgumentException(s"unknown constant pool tag $tag")
      }
    ).dropWhile(_ < count).next(): Unit
    input.skipNBytes(2)
    val owner = constants(classes(input.readUnsignedShort()))
    input.skipNBytes(2)
    input.skipNBytes(2L * input.readUnsignedShort())
    members(input, constants): Unit
    members(input, constants).map((name, size) => Method(owner, name, size))
  }

  /** The name and code length of each field or method in the table at the stream's position. */
  private def members(input: DataInputStream, constants: collection.Map[Int, String]): Vector[(String, Int)] =
    Vector.fill(input.readUnsignedShort()) {
      input.skipNBytes(2)
      val name = constants(input.readUnsignedShort())
      input.skipNBytes(2)
      val sizes = Vector.fill(input.readUnsignedShort()) {
        val attribute = constants(input.readUnsignedShort())
        val length    = input.readInt()
        if (attribute == "Code") {
          input.skipNBytes(4)
          val size = input.readInt()
          input.skipNBytes(length - 8L)
          Some(size)
        } else {
          input.skipNBytes(length.toLong)
          None
        }
      }
      sizes.flatten.headOption.map(name -> _)
    }.flatten
}
