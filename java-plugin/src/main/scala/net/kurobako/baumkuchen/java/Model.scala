package net.kurobako.baumkuchen.java

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** A reference from one type to another, which the graph of the build holds.
  *
  * @param from
  *   the type that makes the reference
  * @param to
  *   the type the reference names
  */
case class Edge(from: String, to: String)

/** The model the javac plugin collects, which the analysis reads: the types of the code, the methods and the fields
  * they hold, and the calls between them.
  *
  * The format is one record a line, with the fields separated by tabs, which is the format the manifest of the analysis
  * uses:
  *
  * {{{
  * node<TAB>name<TAB>source<TAB>line<TAB>exported<TAB>outer<TAB>suppressed<TAB>entry<TAB>generated
  * method<TAB>owner<TAB>name<TAB>exported<TAB>stored<TAB>parameters<TAB>location<TAB>returns<TAB>overriding
  * field<TAB>owner<TAB>name<TAB>kind<TAB>shape<TAB>cases<TAB>primitive
  * site<TAB>caller<TAB>callee<TAB>arguments<TAB>location<TAB>discarded
  * edge<TAB>from<TAB>to
  * lint<TAB>id<TAB>owner<TAB>method<TAB>path<TAB>line<TAB>detail
  * }}}
  *
  * The name of a node is its qualified name, which is the package, the types that hold it and its own name, joined by
  * dots, the way the analysis names the nodes of Scala code. The source is the path of the file the node is defined in,
  * and the line the line it is defined on. A node that nothing refers to is reported as unused.
  *
  * A parameter of a method and a field of a type are written the same way: the name, the kind of the type, and the
  * shape of the type, which is what the analysis compares the parameters of the methods of a build by. The kind is one
  * of `flag`, `number`, `optional`, `tag` and `other`, and the shape is the name of the type, empty for a flag.
  *
  * A method holds the type it returns, which the analysis reads to know whether a call to it gives a value, and whether
  * it overrides a method of a supertype, which the analysis reads as a method it inherits rather than declares. A call
  * holds whether the code that makes it drops its result, which is how the analysis knows that nothing uses the value
  * the call gives.
  *
  * A lint is what the plugin finds wrong with the code of a file, which the analysis reports as a finding of its own:
  * the id names the lint, the owner is the type that holds the code, the method is the method that holds it, and the
  * path and the line are where the code is.
  */
object Model {

  /** A node of the model: a type. */
  case class Node(
      name: String,
      source: String,
      line: Int,
      exported: Boolean,
      outer: Option[String],
      suppressed: Set[String],
      entry: Boolean = false,
      generated: Boolean = false
  )

  /** A method of a type, with the parameters it takes, the type it returns and whether it overrides a method of a
    * supertype.
    */
  case class Method(
      owner: String,
      name: String,
      exported: Boolean,
      stored: Set[Int],
      parameters: Vector[Parameter],
      location: String,
      returns: String = "",
      overriding: Boolean = false,
      suppressed: Set[String] = Set.empty
  )

  /** A parameter of a method, or a field of a type. */
  case class Parameter(name: String, kind: String, shape: String, cases: Vector[String], primitive: Boolean)

  /** A field of a type, which the analysis makes a field of the product it makes of the type. */
  case class Field(owner: String, parameter: Parameter)

  /** A call, from the method that makes it to the one it calls, whose result is dropped when the code does not use it,
    * and which the code inverts with `!` when `negated` is set.
    */
  case class Site(
      caller: String,
      callee: String,
      arguments: Vector[String],
      location: String,
      discarded: Boolean = false,
      negated: Boolean = false
  )

  /** A class the code names only in a text, which a module build can look up at run time but the compiler cannot see.
    */
  case class Reflection(caller: String, target: String, location: String)

  /** The characters of a value the format of the model cannot hold as they are, which are written as their code. */
  private val Codes: Map[Char, String] =
    Map('\t' -> "\\x09", '\n' -> "\\x0a", '\r' -> "\\x0d", '\\' -> "\\x5c", ',' -> "\\x2c")

  /** The value of an argument, which the format of the model cannot hold as it is, written as the codes of the
    * characters that would break a record of it.
    */
  def encode(value: String): String =
    value.flatMap(character => Codes.get(character).fold(character.toString)(code => code))

  /** The value an argument holds, which the model wrote as the codes of the characters that would break a record. */
  def decode(value: String): String = {
    // The code of the backslash is read last, as the codes of the other characters hold one.
    val decoded = Codes.foldLeft(value) { case (text, (character, code)) =>
      if character == '\\' then text else text.replace(code, character.toString)
    }
    decoded.replace("\\x5c", "\\")
  }

  /** A product of the Java code, which is a record or a class whose fields are its shape. */
  case class Product(node: String, name: String, fields: Vector[Parameter], parents: Set[String], exported: Boolean)

  /** A sum of the Java code, which is a sealed type and the types that extend it. */
  case class Sum(node: String, name: String, variants: Vector[Variant])

  /** A variant of a sum, which is a type that extends a sealed one. */
  case class Variant(name: String, fields: Vector[Parameter])

  /** A lint of the Java code, which the analysis reports as a finding of its own.
    *
    * @param id
    *   the id the lint is reported under, which the analysis and the user name it by
    * @param owner
    *   the name of the type that holds the code the lint is about, which is empty when the lint is of the file itself
    * @param method
    *   the name of the method that holds the code, which is empty when the lint is of the type or of the file
    * @param path
    *   the file the code is in
    * @param line
    *   the line the code is on
    * @param detail
    *   what is wrong with the code
    */
  case class Lint(id: String, owner: String, method: String, path: String, line: Int, detail: String)

  /** A fragment of code, with the hashes that tell the copies of it apart. The `exact` hash takes the values of the
    * literals into account, while the `loose` hash ignores them, which is what the near duplicates differ in. The
    * literals are the values of the fragment, in the order they are met, which the holes of a near duplicate are made
    * of.
    */
  case class Fragment(
      node: String,
      path: String,
      start: Int,
      end: Int,
      from: Int,
      to: Int,
      cost: Int,
      exact: Long,
      loose: Long,
      literals: Vector[String]
  )

  /** The model of a compilation: the nodes it holds, the methods and the fields, and the calls between them. */
  case class Model(
      nodes: Vector[Node],
      methods: Vector[Method],
      fields: Vector[Field],
      sites: Vector[Site],
      edges: Vector[Edge] = Vector.empty,
      fragments: Vector[Fragment] = Vector.empty,
      products: Vector[Product] = Vector.empty,
      sums: Vector[Sum] = Vector.empty,
      lints: Vector[Lint] = Vector.empty,
      reflections: Vector[Reflection] = Vector.empty
  ) {

    /** The model as the lines the analysis reads. */
    def lines: Vector[String] = {
      val nodes = this.nodes.map { node =>
        Vector(
          "node",
          node.name,
          node.source,
          node.line.toString,
          node.exported.toString,
          node.outer.getOrElse(""),
          node.suppressed.toSeq.sorted.mkString(","),
          node.entry.toString,
          node.generated.toString
        ).mkString("\t")
      }
      val methods = this.methods.map { method =>
        Vector(
          "method",
          method.owner,
          method.name,
          method.exported.toString,
          method.stored.toSeq.sorted.mkString(","),
          method.parameters.map(parameter).mkString(";"),
          method.location,
          method.returns,
          method.overriding.toString,
          method.suppressed.toSeq.sorted.mkString(",")
        ).mkString("\t")
      }
      val fields = this.fields.map { field =>
        Vector(
          "field",
          field.owner,
          field.parameter.name,
          field.parameter.kind,
          field.parameter.shape,
          field.parameter.cases.mkString(","),
          field.parameter.primitive.toString
        ).mkString("\t")
      }
      val sites = this.sites.map { site =>
        Vector(
          "site",
          site.caller,
          site.callee,
          site.arguments.map(encode).mkString(","),
          site.location,
          site.discarded.toString,
          site.negated.toString
        ).mkString("\t")
      }
      val edges     = this.edges.map(edge => Vector("edge", edge.from, edge.to).mkString("\t"))
      val fragments = this.fragments.map { fragment =>
        Vector(
          "clone",
          fragment.node,
          fragment.path,
          fragment.start.toString,
          fragment.end.toString,
          fragment.from.toString,
          fragment.to.toString,
          fragment.cost.toString,
          fragment.exact.toString,
          fragment.loose.toString,
          fragment.literals.map(escape).mkString(",")
        ).mkString("\t")
      }
      val products = this.products.map { product =>
        Vector(
          "product",
          product.node,
          product.name,
          product.fields.map(parameter).mkString(";"),
          product.parents.toVector.sorted.mkString(","),
          product.exported.toString
        ).mkString("\t")
      }
      val sums = this.sums.map { sum =>
        Vector(
          "sum",
          sum.node,
          sum.name,
          sum.variants.map(_.name).mkString(";")
        ).mkString("\t")
      }
      val lints = this.lints.map { lint =>
        Vector(
          "lint",
          lint.id,
          lint.owner,
          lint.method,
          lint.path,
          lint.line.toString,
          escape(lint.detail)
        ).mkString("\t")
      }
      val reflections = this.reflections.map { reflection =>
        Vector("reflect", reflection.caller, reflection.target, reflection.location).mkString("\t")
      }
      nodes ++ methods ++ fields ++ sites ++ edges ++ fragments ++ products ++ sums ++ lints ++ reflections
    }
  }

  /** A parameter as the line the analysis reads writes it. */
  private def parameter(parameter: Parameter): String =
    s"${parameter.name}|${parameter.kind}|${parameter.shape}|${parameter.cases.mkString(",")}|${parameter.primitive}"

  /** A parameter as the line the analysis reads holds it. */
  private def held(line: String): Parameter =
    line.split("\\|", -1) match {
      case Array(name, kind, shape, cases, primitive) =>
        Parameter(name, kind, shape, cases.split(",").filter(_.nonEmpty).toVector, primitive.toBoolean)
      case _ => Parameter(line, "other", "", Vector.empty, false)
    }

  /** The model of the compilation the plugin ran in, read from the file it wrote, or what is wrong with it. */
  def read(file: Path): Either[String, Model] = {
    val lines = Try(Files.readAllLines(file, StandardCharsets.UTF_8).asScala.toVector).toEither.left.map {
      error => s"$file could not be read: ${error.getMessage}"
    }
    lines.flatMap { lines =>
      val nodes = lines.collect {
        case line if line.startsWith("node\t") =>
          val fields = line.split("\t", -1)
          if fields.length < 5 then Left(s"$file: a node holds at least a name, a source and a line")
          else
            Right(
              Node(
                fields(1),
                fields(2),
                fields(3).toIntOption.getOrElse(0),
                fields(4).toBoolean,
                fields.lift(5).filter(_.nonEmpty),
                fields.lift(6).filter(_.nonEmpty).toSet.flatMap(_.split(",").filter(_.nonEmpty)),
                fields.lift(7).exists(_.toBoolean),
                fields.lift(8).exists(_.toBoolean)
              )
            )
      }
      val methods = lines.collect {
        case line if line.startsWith("method\t") =>
          val fields = line.split("\t", -1)
          if fields.length < 7 then Left(s"$file: a method holds an owner, a name and its parameters")
          else
            Right(
              Method(
                fields(1),
                fields(2),
                fields(3).toBoolean,
                fields(4).split(",").filter(_.nonEmpty).flatMap(_.toIntOption).toSet,
                fields(5).split(";").filter(_.nonEmpty).map(held).toVector,
                fields(6),
                fields.lift(7).getOrElse(""),
                fields.lift(8).exists(_.toBoolean),
                fields.lift(9).fold(Set.empty[String])(_.split(",").filter(_.nonEmpty).toSet)
              )
            )
      }
      val fields = lines.collect {
        case line if line.startsWith("field\t") =>
          val parts = line.split("\t", -1)
          if parts.length < 7 then Left(s"$file: a field holds an owner and a name")
          else
            Right(
              Field(
                parts(1),
                Parameter(
                  parts(2),
                  parts(3),
                  parts(4),
                  parts(5).split(",").filter(_.nonEmpty).toVector,
                  parts(6).toBoolean
                )
              )
            )
      }
      val sites = lines.collect {
        case line if line.startsWith("site\t") =>
          val parts = line.split("\t", -1)
          if parts.length < 5 then Left(s"$file: a call holds a caller and a callee")
          else
            Right(
              Site(
                parts(1),
                parts(2),
                parts(3).split(",").filter(_.nonEmpty).map(decode).toVector,
                parts(4),
                parts.lift(5).exists(_.toBoolean),
                parts.lift(6).exists(_.toBoolean)
              )
            )
      }
      val edges = lines.collect {
        case line if line.startsWith("edge\t") =>
          val parts = line.split("\t", -1)
          if parts.length < 3 then Left(s"$file: a reference holds a source and a target")
          else Right(Edge(parts(1), parts(2)))
      }
      val fragments = lines.collect {
        case line if line.startsWith("clone\t") =>
          val parts = line.split("\t", -1)
          if parts.length < 11 then Left(s"$file: a fragment holds a node, a place and the hashes of it")
          else
            Right(
              Fragment(
                parts(1),
                parts(2),
                parts(3).toIntOption.getOrElse(0),
                parts(4).toIntOption.getOrElse(0),
                parts(5).toIntOption.getOrElse(0),
                parts(6).toIntOption.getOrElse(0),
                parts(7).toIntOption.getOrElse(0),
                parts(8).toLongOption.getOrElse(0L),
                parts(9).toLongOption.getOrElse(0L),
                parts(10).split(",", -1).toVector.filter(_.nonEmpty).map(unescape)
              )
            )
      }
      val products = lines.collect {
        case line if line.startsWith("product\t") =>
          val parts = line.split("\t", -1)
          if parts.length < 6 then Left(s"$file: a product holds a node, a name and its fields")
          else
            Right(
              Product(
                parts(1),
                parts(2),
                parts(3).split(";", -1).toVector.filter(_.nonEmpty).map(held).toVector,
                parts(4).split(",", -1).toVector.filter(_.nonEmpty).toSet,
                parts(5).toBoolean
              )
            )
      }
      val sums = lines.collect {
        case line if line.startsWith("sum\t") =>
          val parts = line.split("\t", -1)
          if parts.length < 4 then Left(s"$file: a sum holds a node, a name and its variants")
          else
            Right(
              Sum(
                parts(1),
                parts(2),
                parts(3).split(";", -1).toVector.filter(_.nonEmpty).map(name => Variant(name, Vector.empty))
              )
            )
      }
      val lints = lines.collect {
        case line if line.startsWith("lint\t") =>
          val parts = line.split("\t", -1)
          if parts.length < 7 then Left(s"$file: a lint holds an id, an owner, a method, a path, a line and a detail")
          else
            Right(
              Lint(
                parts(1),
                parts(2),
                parts(3),
                parts(4),
                parts(5).toIntOption.getOrElse(0),
                unescape(parts(6))
              )
            )
      }
      val reflections = lines.collect {
        case line if line.startsWith("reflect\t") =>
          val parts = line.split("\t", -1)
          if parts.length < 4 then Left(s"$file: a reflective lookup holds a caller, a target and a place")
          else Right(Reflection(parts(1), parts(2), parts(3)))
      }
      val failures =
        (nodes ++ methods ++ fields ++ sites ++ edges ++ fragments ++ products ++ sums ++ lints ++ reflections)
          .collect { case Left(failure) => failure }
      if failures.nonEmpty then Left(failures.mkString("\n"))
      else
        Right(
          Model(
            nodes.collect { case Right(node) => node },
            methods.collect { case Right(method) => method },
            fields.collect { case Right(field) => field },
            sites.collect { case Right(site) => site },
            edges.collect { case Right(edge) => edge },
            fragments.collect { case Right(fragment) => fragment },
            products.collect { case Right(product) => product },
            sums.collect { case Right(sum) => sum },
            lints.collect { case Right(lint) => lint },
            reflections.collect { case Right(reflection) => reflection }
          )
        )
    }
  }

  /** A literal, which the holes of a near duplicate are made of, escaped so that the format holds it whole. */
  def escape(value: String): String =
    value.flatMap {
      case '\\'  => "\\\\"
      case ','   => "\\c"
      case '\t'  => "\\t"
      case '\n'  => "\\n"
      case other => other.toString
    }

  /** A literal the format held whole. */
  def unescape(value: String): String = {
    val text  = new StringBuilder
    var index = 0
    while index < value.length do
      if value.charAt(index) == '\\' && index + 1 < value.length then {
        value.charAt(index + 1) match {
          case '\\'  => text += '\\'
          case 'c'   => text += ','
          case 't'   => text += '\t'
          case 'n'   => text += '\n'
          case other => text += other
        }
        index += 2
      } else {
        text += value.charAt(index)
        index += 1
      }
    text.toString
  }

  /** Writes the model to the file, which the analysis reads. */
  def write(model: Model, file: Path): Unit = {
    Files.createDirectories(file.getParent)
    Files.writeString(file, model.lines.mkString("", "\n", "\n"), StandardCharsets.UTF_8)
  }
}
