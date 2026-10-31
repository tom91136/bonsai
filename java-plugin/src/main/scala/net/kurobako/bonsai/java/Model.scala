package net.kurobako.bonsai.java

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
  * cost<TAB>owner<TAB>name<TAB>key<TAB>line<TAB>suppressed<TAB>parameters<TAB>tree
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

  /** A product of the Java code, which is a record or a class whose fields are its shape. */
  case class Product(node: String, name: String, fields: Vector[Parameter], parents: Set[String], exported: Boolean)

  /** A sum of the Java code, which is a sealed type and the types that extend it. */
  case class Sum(node: String, name: String, variants: Vector[Variant])

  /** A variant of a sum, which is a type that extends a sealed one. */
  case class Variant(name: String)

  /** How a guard reads a field: an optional for presence, a flag for truth. */
  enum Probe {
    case Present, Flag
  }

  enum Junction {
    case And, Or
  }

  /** What a constructor of a type requires of the values it is given, over the names of the fields they are stored in.
    * The parts the analysis has no vocabulary for are `Unknown`.
    */
  enum Guard {
    case Known(value: Boolean)
    case Holds(field: String, probe: Probe)
    case Is(field: String, value: String)
    case Compare(field: String, operator: String, value: BigDecimal)
    case Not(guard: Guard)
    case Connective(junction: Junction, left: Guard, right: Guard)
    case Unknown

    /** The guard as the text the analysis reads it from. */
    def render: String = this match {
      case Known(value)                   => if value then "T" else "F"
      case Holds(field, Probe.Present)    => s"(present $field)"
      case Holds(field, Probe.Flag)       => s"(flag $field)"
      case Is(field, value)               => s"(is $field $value)"
      case Compare(field, operator, at)   => s"(compare $field $operator ${at.bigDecimal.toPlainString})"
      case Not(guard)                     => s"(not ${guard.render})"
      case Connective(Junction.And, l, r) => s"(and ${l.render} ${r.render})"
      case Connective(Junction.Or, l, r)  => s"(or ${l.render} ${r.render})"
      case Unknown                        => "?"
    }

    def and(that: Guard): Guard = Connective(Junction.And, this, that)
    def or(that: Guard): Guard  = Connective(Junction.Or, this, that)
  }

  object Guard {

    /** The guard the text holds, if it holds one. */
    def parse(text: String): Option[Guard] = {
      val tokens = text.replace("(", " ( ").replace(")", " ) ").trim.split("\\s+").toList.filter(_.nonEmpty)
      def one(rest: List[String]): Option[(Guard, List[String])] = rest match {
        case "T" :: tail                                                   => Some((Known(true), tail))
        case "F" :: tail                                                   => Some((Known(false), tail))
        case "?" :: tail                                                   => Some((Unknown, tail))
        case "(" :: "present" :: field :: ")" :: tail                      => Some((Holds(field, Probe.Present), tail))
        case "(" :: "flag" :: field :: ")" :: tail                         => Some((Holds(field, Probe.Flag), tail))
        case "(" :: "is" :: field :: value :: ")" :: tail                  => Some((Is(field, value), tail))
        case "(" :: "compare" :: field :: operator :: value :: ")" :: tail =>
          value.toDoubleOption.map(_ => (Compare(field, operator, BigDecimal(value)), tail))
        case "(" :: "not" :: tail =>
          one(tail).collect { case (inner, ")" :: after) => (Not(inner), after) }
        case "(" :: (name @ ("and" | "or")) :: tail =>
          one(tail).flatMap { (left, afterLeft) =>
            one(afterLeft).collect { case (right, ")" :: after) =>
              (Connective(if name == "and" then Junction.And else Junction.Or, left, right), after)
            }
          }
        case _ => None
      }
      one(tokens).collect { case (guard, Nil) => guard }
    }
  }

  /** How a type is constructed: who may call its constructors (`open`, `private` for a type whose constructors are all
    * private, or `factory` for a type with the static `factories` that return an `Optional` of it), and the guard its
    * constructor starts with.
    */
  case class Construction(owner: String, access: String, factories: Vector[String], guard: Guard)

  /** What a method requires of its arguments before it does anything else, over the names of its parameters. */
  case class Precondition(method: String, guard: Guard)

  /** A field that is not private, final or volatile and either starts with a value (`initialised`) or is assigned once
    * by every constructor of its type, so that it can be final unless some code of the build writes it, which `writes`
    * lists; `exported` when other builds can reach it.
    */
  case class Reassignable(
      owner: String,
      name: String,
      exported: Boolean,
      path: String,
      line: Int,
      suppressed: Set[String] = Set.empty,
      initialised: Boolean = true
  )

  /** An assignment, compound assignment, increment or decrement of a field; `constructor` for a plain assignment in a
    * constructor of the type that declares the field.
    */
  case class Write(owner: String, name: String, constructor: Boolean = false)

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
      literals: Vector[String],
      member: String = "",
      broad: Long = 0L,
      variables: Vector[Clones.Variable] = Vector.empty,
      roles: Int = Roles.Whole,
      reach: String = ""
  )

  /** What a fragment is a copy of, as bits of its `roles`: a `Whole` run of statements is the copy of larger code, the
    * rest are expressions, which are copies only of what the others say. `Small` is a chain of calls, `Entire` the
    * single expression a method consists of, `Tiny` such a body too small to be a copy of its own, `Helper` such a body
    * that calls of the method may replace, and `Direct` a single call. The `broad` hash leaves out the references and
    * literals that the `variables` list, `member` names the method the fragment is in, and `reach` is the type it is
    * visible to when it is not visible to all code.
    */
  object Roles {
    val Small  = 1
    val Tiny   = 2
    val Entire = 4
    val Direct = 8
    val Helper = 16
    val Whole  = 32
  }

  /** The unevaluated cost of a method with a body: the loops it runs, the library work it does and the calls it makes,
    * which the analysis evaluates against the bounds of the callees. The `key` is the qualified owner, the name and the
    * erased JVM descriptor, which tells overloads apart and is the key the analysis finds a Scala method by.
    *
    * A tree is written as nodes in braces, separated by spaces: `{loop line extent nodes...}` for a loop of that many
    * turns that holds the nodes, `{each line extent nodes...}` for a function run once per element of a collection,
    * `{op line kind extent}` for library work over a collection, and `{call line key receiver arguments...}` for a call
    * of a method of the build. A branch of a method that calls itself and holds no such call is `{exit line nodes...}`:
    * it ends the recursion, so it runs once rather than in every frame. An extent is a path of positions and fields,
    * such as `$0`, `$1.items` or `this.items`, and `?` where the collection is not one. In a call of a method to
    * itself, an argument that is a strictly smaller part of the parameter in its position (a sublist, a substring) is
    * `$i<`, and an integer parameter less a positive constant is `$i-`.
    */
  case class Cost(
      owner: String,
      name: String,
      key: String,
      line: Int,
      suppressed: Set[String],
      parameters: Vector[String],
      body: Vector[Cost.Node]
  )

  object Cost {
    val Unknown = "?"
    val Static  = "-"
    val Fixed   = "1"

    def smaller(index: Int): String = s"$$$index<"

    def stepped(index: Int): String = s"$$$index-"

    /** The library work an operation does: a linear search, a sort, a copy of a collection, a shift of the elements of
      * a list, or the growth of a text over `depth` loops.
      */
    enum Kind {
      case Search, Sort, Copy, Shift
      case Concat(depth: Int)

      def render: String = this match {
        case Concat(depth) => s"concat:$depth"
        case other         => other.toString.toLowerCase
      }
    }

    object Kind {
      def parse(text: String): Option[Kind] = text match {
        case "search"                               => Some(Search)
        case "sort"                                 => Some(Sort)
        case "copy"                                 => Some(Copy)
        case "shift"                                => Some(Shift)
        case concat if concat.startsWith("concat:") => concat.drop(7).toIntOption.map(Concat(_))
        case _                                      => None
      }
    }

    /** A step of the cost of a method, with the line it is on. */
    case class Node(line: Int, step: Step) {
      def render: String = step match {
        case Step.Loop(extent, callback, absorbs, body) =>
          val head = (if callback then "{each" else "{loop") + (if absorbs > 0 then s":$absorbs" else "")
          (Vector(head, line.toString, extent) ++ body.map(_.render)).mkString(" ") + "}"
        case Step.Exit(body)       => (Vector("{exit", line.toString) ++ body.map(_.render)).mkString(" ") + "}"
        case Step.Op(kind, extent) => s"{op $line ${kind.render} $extent}"
        case Step.Call(key, receiver, arguments) =>
          (Vector("{call", line.toString, key, receiver) ++ arguments).mkString(" ") + "}"
      }
    }

    enum Step {
      case Loop(extent: String, callback: Boolean, absorbs: Int, body: Vector[Node])
      case Exit(body: Vector[Node])
      case Op(kind: Kind, extent: String)
      case Call(key: String, receiver: String, arguments: Vector[String])
    }

    def render(nodes: Vector[Node]): String = nodes.map(_.render).mkString(" ")

    private val loopHead = "(loop|each)(:\\d+)?".r

    /** The nodes a text written by `render` holds. */
    def parse(text: String): Option[Vector[Node]] = {
      val tokens = text.replace("{", " { ").replace("}", " } ").trim.split("\\s+").toList.filter(_.nonEmpty)
      def nodes(rest: List[String], acc: Vector[Node]): Option[(Vector[Node], List[String])] = rest match {
        case "{" :: tail =>
          node(tail).flatMap((found, after) => nodes(after, acc :+ found))
        case _ => Some((acc, rest))
      }
      def node(rest: List[String]): Option[(Node, List[String])] = rest match {
        case head :: line :: extent :: tail if loopHead.matches(head) =>
          for {
            number        <- line.toIntOption
            (body, after) <- nodes(tail, Vector.empty)
            remaining     <- after match { case "}" :: more => Some(more); case _ => None }
          } yield (
            Node(number, Step.Loop(extent, head.startsWith("each"), head.drop(5).toIntOption.getOrElse(0), body)),
            remaining
          )
        case "exit" :: line :: tail =>
          for {
            number        <- line.toIntOption
            (body, after) <- nodes(tail, Vector.empty)
            remaining     <- after match { case "}" :: more => Some(more); case _ => None }
          } yield (Node(number, Step.Exit(body)), remaining)
        case "op" :: line :: kind :: extent :: "}" :: tail =>
          for {
            number <- line.toIntOption
            parsed <- Kind.parse(kind)
          } yield (Node(number, Step.Op(parsed, extent)), tail)
        case "call" :: line :: key :: receiver :: tail =>
          val (arguments, after) = tail.span(_ != "}")
          for {
            number <- line.toIntOption
            _      <- after.headOption
          } yield (Node(number, Step.Call(key, receiver, arguments.toVector)), after.tail)
        case _ => None
      }
      nodes(tokens, Vector.empty).collect { case (found, Nil) => found }
    }
  }

  private final case class Codec[A](
      tag: String,
      least: Int,
      message: String,
      write: A => Vector[String],
      read: Array[String] => Either[String, A]
  ) {
    def lines(values: Vector[A]): Vector[String] = values.map(value => (tag +: write(value)).mkString("\t"))
  }

  private def items(text: String, separator: String = ","): Vector[String] =
    text.split(separator, -1).toVector.filter(_.nonEmpty)

  private def all[A](values: Vector[Either[String, A]]): Either[String, Vector[A]] = {
    val (failures, found) = values.partitionMap(identity)
    failures.headOption.toLeft(found)
  }

  private def parameterColumns(parameter: Parameter): Vector[String] =
    Vector(parameter.name, parameter.kind, parameter.shape, parameter.cases.mkString(","), parameter.primitive.toString)

  private def parameterOf(columns: Array[String]): Either[String, Parameter] =
    columns match {
      case Array(name, kind, shape, cases, primitive) =>
        primitive.toBooleanOption
          .map(Parameter(name, kind, shape, items(cases), _))
          .toRight(s"a parameter holds a boolean for primitive, not $primitive")
      case _ => Left("a parameter holds a name, a kind, a shape, its cases and whether it is primitive")
    }

  private def parameterText(parameters: Vector[Parameter]): String =
    parameters.map(parameter => parameterColumns(parameter).mkString("|")).mkString(";")

  private def parameterList(text: String): Either[String, Vector[Parameter]] =
    all(items(text, ";").map(parameter => parameterOf(parameter.split("\\|", -1))))

  private def variableText(variable: Clones.Variable): String =
    Vector(variable.kind.toString, variable.text, variable.shown, variable.parameter.toString, variable.suffix)
      .map(escape)
      .mkString("|")

  private def variableList(text: String): Either[String, Vector[Clones.Variable]] =
    all(items(text).map(item => variableOf(item.split("\\|", -1).map(unescape))))

  private def variableOf(columns: Array[String]): Either[String, Clones.Variable] =
    columns match {
      case Array(kind, text, shown, parameter, suffix) if kind.length == 1 =>
        parameter.toIntOption
          .map(Clones.Variable(kind.charAt(0), text, shown, _, suffix))
          .toRight(s"a variable holds a whole number for its parameter, not $parameter")
      case _ => Left("a variable holds a kind, a text, a shown text, a parameter and a suffix")
    }

  private val nodeCodec = Codec[Node](
    "node",
    4,
    "a node holds at least a name, a source and a line",
    node =>
      Vector(
        node.name,
        node.source,
        node.line.toString,
        node.exported.toString,
        node.outer.getOrElse(""),
        node.suppressed.toSeq.sorted.mkString(","),
        node.entry.toString,
        node.generated.toString
      ),
    columns =>
      Right(
        Node(
          columns(0),
          columns(1),
          columns(2).toInt,
          columns(3).toBoolean,
          columns.lift(4).filter(_.nonEmpty),
          items(columns.lift(5).getOrElse("")).toSet,
          columns.lift(6).exists(_.toBoolean),
          columns.lift(7).exists(_.toBoolean)
        )
      )
  )

  private val methodCodec = Codec[Method](
    "method",
    6,
    "a method holds an owner, a name and its parameters",
    method =>
      Vector(
        method.owner,
        method.name,
        method.exported.toString,
        method.stored.toSeq.sorted.mkString(","),
        parameterText(method.parameters),
        method.location,
        method.returns,
        method.overriding.toString,
        method.suppressed.toSeq.sorted.mkString(",")
      ),
    columns =>
      parameterList(columns(4)).map(parameters =>
        Method(
          columns(0),
          columns(1),
          columns(2).toBoolean,
          items(columns(3)).map(_.toInt).toSet,
          parameters,
          columns(5),
          columns.lift(6).getOrElse(""),
          columns.lift(7).exists(_.toBoolean),
          items(columns.lift(8).getOrElse("")).toSet
        )
      )
  )

  private val fieldCodec = Codec[Field](
    "field",
    6,
    "a field holds an owner and a name",
    field => field.owner +: parameterColumns(field.parameter),
    columns => parameterOf(columns.slice(1, 6)).map(Field(columns(0), _))
  )

  private val siteCodec = Codec[Site](
    "site",
    4,
    "a call holds a caller and a callee",
    site =>
      Vector(
        site.caller,
        site.callee,
        site.arguments.map(escape).mkString(","),
        site.location,
        site.discarded.toString,
        site.negated.toString
      ),
    columns =>
      Right(
        Site(
          columns(0),
          columns(1),
          items(columns(2)).map(unescape),
          columns(3),
          columns.lift(4).exists(_.toBoolean),
          columns.lift(5).exists(_.toBoolean)
        )
      )
  )

  private val edgeCodec = Codec[Edge](
    "edge",
    2,
    "a reference holds a source and a target",
    edge => Vector(edge.from, edge.to),
    columns => Right(Edge(columns(0), columns(1)))
  )

  private val cloneCodec = Codec[Fragment](
    "clone",
    10,
    "a fragment holds a node, a place and the hashes of it",
    fragment =>
      Vector(
        fragment.node,
        fragment.path,
        fragment.start.toString,
        fragment.end.toString,
        fragment.from.toString,
        fragment.to.toString,
        fragment.cost.toString,
        fragment.exact.toString,
        fragment.loose.toString,
        fragment.literals.map(escape).mkString(","),
        fragment.member,
        fragment.broad.toString,
        fragment.variables.map(variableText).mkString(","),
        fragment.roles.toString,
        fragment.reach
      ),
    columns =>
      variableList(columns.lift(12).getOrElse("")).map(variables =>
        Fragment(
          columns(0),
          columns(1),
          columns(2).toInt,
          columns(3).toInt,
          columns(4).toInt,
          columns(5).toInt,
          columns(6).toInt,
          columns(7).toLong,
          columns(8).toLong,
          items(columns(9)).map(unescape),
          columns.lift(10).getOrElse(""),
          columns.lift(11).map(_.toLong).getOrElse(0L),
          variables,
          columns.lift(13).map(_.toInt).getOrElse(Roles.Whole),
          columns.lift(14).getOrElse("")
        )
      )
  )

  private val productCodec = Codec[Product](
    "product",
    5,
    "a product holds a node, a name and its fields",
    product =>
      Vector(
        product.node,
        product.name,
        parameterText(product.fields),
        product.parents.toSeq.sorted.mkString(","),
        product.exported.toString
      ),
    columns =>
      parameterList(columns(2)).map(fields =>
        Product(columns(0), columns(1), fields, items(columns(3)).toSet, columns(4).toBoolean)
      )
  )

  private val sumCodec = Codec[Sum](
    "sum",
    3,
    "a sum holds a node, a name and its variants",
    sum => Vector(sum.node, sum.name, sum.variants.map(_.name).mkString(";")),
    columns => Right(Sum(columns(0), columns(1), items(columns(2), ";").map(Variant(_))))
  )

  private val lintCodec = Codec[Lint](
    "lint",
    6,
    "a lint holds an id, an owner, a method, a path, a line and a detail",
    lint => Vector(lint.id, lint.owner, lint.method, lint.path, lint.line.toString, escape(lint.detail)),
    columns =>
      Right(
        Lint(columns(0), columns(1), columns(2), columns(3), columns(4).toInt, unescape(columns(5)))
      )
  )

  private val reflectCodec = Codec[Reflection](
    "reflect",
    3,
    "a reflective lookup holds a caller, a target and a place",
    reflection => Vector(reflection.caller, escape(reflection.target), reflection.location),
    columns => Right(Reflection(columns(0), unescape(columns(1)), columns(2)))
  )

  private val constructionCodec = Codec[Construction](
    "construction",
    4,
    "a construction holds an owner, an access, its factories and a guard",
    construction =>
      Vector(construction.owner, construction.access, construction.factories.mkString(","), construction.guard.render),
    columns =>
      Right(Construction(columns(0), columns(1), items(columns(2)), Guard.parse(columns(3)).getOrElse(Guard.Unknown)))
  )

  private val preconditionCodec = Codec[Precondition](
    "precondition",
    2,
    "a precondition holds a method and a guard",
    precondition => Vector(precondition.method, precondition.guard.render),
    columns => Right(Precondition(columns(0), Guard.parse(columns(1)).getOrElse(Guard.Unknown)))
  )

  private val reassignableCodec = Codec[Reassignable](
    "reassignable",
    7,
    "a reassignable field holds an owner, a name, a place and what it suppresses",
    field =>
      Vector(
        field.owner,
        field.name,
        field.exported.toString,
        field.path,
        field.line.toString,
        field.suppressed.toSeq.sorted.mkString(","),
        field.initialised.toString
      ),
    columns =>
      Right(
        Reassignable(
          columns(0),
          columns(1),
          columns(2).toBoolean,
          columns(3),
          columns(4).toInt,
          items(columns(5)).toSet,
          columns(6).toBoolean
        )
      )
  )

  private val writeCodec = Codec[Write](
    "write",
    3,
    "a write holds an owner, a name and where it is",
    write => Vector(write.owner, write.name, write.constructor.toString),
    columns => Right(Write(columns(0), columns(1), columns(2).toBoolean))
  )

  private val costCodec = Codec[Cost](
    "cost",
    7,
    "a cost holds an owner, a name, a key, a line, what it suppresses and a tree",
    cost =>
      Vector(
        cost.owner,
        cost.name,
        cost.key,
        cost.line.toString,
        cost.suppressed.toSeq.sorted.mkString(","),
        cost.parameters.mkString(","),
        Cost.render(cost.body)
      ),
    columns =>
      Cost.parse(columns(6)).toRight(s"the tree of the cost of ${columns(2)} is malformed").map { body =>
        Cost(
          columns(0),
          columns(1),
          columns(2),
          columns(3).toInt,
          items(columns(4)).toSet,
          items(columns(5)),
          body
        )
      }
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
      reflections: Vector[Reflection] = Vector.empty,
      constructions: Vector[Construction] = Vector.empty,
      preconditions: Vector[Precondition] = Vector.empty,
      reassignables: Vector[Reassignable] = Vector.empty,
      writes: Vector[Write] = Vector.empty,
      costs: Vector[Cost] = Vector.empty
  ) {

    /** The model as the lines the analysis reads. */
    def lines: Vector[String] =
      nodeCodec.lines(nodes) ++ methodCodec.lines(methods) ++ fieldCodec.lines(fields) ++ siteCodec.lines(sites) ++
        edgeCodec.lines(edges) ++ cloneCodec.lines(fragments) ++ productCodec.lines(products) ++
        sumCodec.lines(sums) ++ lintCodec.lines(lints) ++ reflectCodec.lines(reflections) ++
        constructionCodec.lines(constructions) ++ preconditionCodec.lines(preconditions) ++
        reassignableCodec.lines(reassignables) ++ writeCodec.lines(writes) ++ costCodec.lines(costs)
  }

  /** The model of the compilation the plugin ran in, read from the file it wrote, or what is wrong with it. */
  def read(file: Path): Either[String, Model] = {
    val lines = Try(Files.readAllLines(file, StandardCharsets.UTF_8).asScala.toVector).toEither.left.map {
      error => s"$file could not be read: ${error.getMessage}"
    }
    lines.flatMap { lines =>
      val rows                                                   = lines.map(_.split("\t", -1)).groupMap(_.head)(_.tail)
      def records[A](codec: Codec[A]): Vector[Either[String, A]] =
        rows.getOrElse(codec.tag, Vector.empty).map { columns =>
          if columns.length < codec.least then Left(s"$file: ${codec.message}")
          else
            try codec.read(columns).left.map(message => s"$file: $message")
            catch { case error: IllegalArgumentException => Left(s"$file: ${codec.message}: ${error.getMessage}") }
        }
      val nodes         = records(nodeCodec)
      val methods       = records(methodCodec)
      val fields        = records(fieldCodec)
      val sites         = records(siteCodec)
      val edges         = records(edgeCodec)
      val fragments     = records(cloneCodec)
      val products      = records(productCodec)
      val sums          = records(sumCodec)
      val lints         = records(lintCodec)
      val reflections   = records(reflectCodec)
      val constructions = records(constructionCodec)
      val preconditions = records(preconditionCodec)
      val reassignables = records(reassignableCodec)
      val writes        = records(writeCodec)
      val costs         = records(costCodec)
      val failures      =
        (nodes ++ methods ++ fields ++ sites ++ edges ++ fragments ++ products ++ sums ++ lints ++ reflections ++
          constructions ++ preconditions ++ reassignables ++ writes ++ costs).collect { case Left(failure) => failure }
      if failures.nonEmpty then Left(failures.mkString("\n"))
      else
        Right(
          Model(
            nodes.flatMap(_.toOption),
            methods.flatMap(_.toOption),
            fields.flatMap(_.toOption),
            sites.flatMap(_.toOption),
            edges.flatMap(_.toOption),
            fragments.flatMap(_.toOption),
            products.flatMap(_.toOption),
            sums.flatMap(_.toOption),
            lints.flatMap(_.toOption),
            reflections.flatMap(_.toOption),
            constructions.flatMap(_.toOption),
            preconditions.flatMap(_.toOption),
            reassignables.flatMap(_.toOption),
            writes.flatMap(_.toOption),
            costs.flatMap(_.toOption)
          )
        )
    }
  }

  /** A text, escaped so that the format holds it whole: the characters that end a field, a list item or a line, the
    * escape itself, and the surrogates that have no pair, which UTF-8 cannot encode.
    */
  private def escape(value: String): String = {
    val text = new StringBuilder
    value.codePoints.toArray.foreach {
      case '\\'                                                                       => text ++= "\\\\"
      case ','                                                                        => text ++= "\\c"
      case '|'                                                                        => text ++= "\\p"
      case '\t'                                                                       => text ++= "\\t"
      case '\n'                                                                       => text ++= "\\n"
      case '\r'                                                                       => text ++= "\\r"
      case lone if lone >= Character.MIN_SURROGATE && lone <= Character.MAX_SURROGATE => text ++= f"\\u$lone%04x"
      case other => text.underlying.appendCodePoint(other)
    }
    text.toString
  }

  /** A text the format held whole. */
  private def unescape(value: String): String = {
    val text  = new StringBuilder
    var index = 0
    while index < value.length do
      if value.charAt(index) == '\\' && index + 1 < value.length then {
        value.charAt(index + 1) match {
          case '\\' => text += '\\'
          case 'c'  => text += ','
          case 'p'  => text += '|'
          case 't'  => text += '\t'
          case 'n'  => text += '\n'
          case 'r'  => text += '\r'
          case 'u'
              if index + 5 < value.length &&
                value.substring(index + 2, index + 6).forall(Character.digit(_, 16) >= 0) =>
            text += Integer.parseInt(value.substring(index + 2, index + 6), 16).toChar
            index += 4
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
    val _ = Files.writeString(file, model.lines.mkString("", "\n", "\n"), StandardCharsets.UTF_8)
  }
}
