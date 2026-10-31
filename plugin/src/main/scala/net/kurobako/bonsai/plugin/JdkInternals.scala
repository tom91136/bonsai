package net.kurobako.bonsai.plugin

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.Symbol
import net.kurobako.bonsai.lifting.Calls

/** The uses of JDK types and members that the JDK does not support, named in the code or by a lookup of a constant
  * name.
  */
private class JdkInternals(checks: Checks) extends tpd.TreeTraverser {

  private val Advice = "a suppression should name the supported alternative or the JDK versions it is keyed on"

  private val Named = Set(
    "java.lang.Class.forName",
    "java.lang.ClassLoader.loadClass",
    "java.lang.invoke.MethodHandles.Lookup.findClass"
  )

  private val Lookups = Set(
    "getField",
    "getDeclaredField",
    "getMethod",
    "getDeclaredMethod",
    "getConstructor",
    "getDeclaredConstructor"
  )

  override def traverse(tree: tpd.Tree)(using Context): Unit = {
    tree match {
      case imported: tpd.Import =>
        val expression = imported.expr
        val reason     =
          if expression.symbol.is(Flags.Package) then JdkPackages.unsupported(expression.symbol.fullName.toString)
          else unsupported(expression.symbol)
        reason.foreach(why => report(s"the ${imported.show} is $why", imported))
      case select: tpd.Select if select.name != nme.CONSTRUCTOR && !named(select.qualifier) =>
        referenced(select.symbol, select)
      case ident: tpd.Ident =>
        referenced(ident.symbol, ident)
      case tpd.Apply(function, arguments) =>
        constantName(arguments).foreach { name =>
          qualifiedName(function.symbol).filter(Named).foreach { qualified =>
            JdkPackages.unsupportedName(name).foreach(why =>
              report(s"the class $name named by $qualified is $why", tree)
            )
          }
        }
        lookup(function, arguments).foreach((member, owner, why) =>
          report(s"the reflective lookup of $member in $owner is $why", tree)
        )
      case _ => ()
    }
    traverseChildren(tree)
  }

  private def qualifiedName(symbol: Symbol)(using Context): Option[String] =
    Option.when(symbol.exists)(Calls.qualified(symbol))

  private def referenced(symbol: Symbol, tree: tpd.Tree)(using Context): Unit =
    unsupported(symbol).foreach(why => report(s"the ${kind(symbol)} ${symbol.showFullName} is $why", tree))

  private def kind(symbol: Symbol)(using Context): String =
    if symbol.isClass || symbol.isType then "type" else "member"

  private def packageOf(symbol: Symbol)(using Context): Option[String] =
    Option.when(symbol.exists && !symbol.is(Flags.Package) && (symbol.isClass || symbol.owner.isClass))(
      symbol.enclosingPackageClass.fullName.toString
    )

  private def unsupported(symbol: Symbol)(using Context): Option[String] =
    packageOf(symbol).flatMap(JdkPackages.unsupported)

  private def named(tree: tpd.Tree)(using Context): Boolean = unsupported(tree.symbol).isDefined

  private def constantName(arguments: List[tpd.Tree]): Option[String] = arguments.headOption.flatMap(constantOf)

  /** The class a receiver names, by a class literal or a lookup of a constant name, and why it is unsupported. */
  private def receiver(tree: tpd.Tree)(using Context): Option[(String, String)] = tree match {
    case tpd.TypeApply(function, List(argument)) if function.symbol.name.toString == "classOf" =>
      val symbol = argument.tpe.typeSymbol
      unsupported(symbol).map(symbol.showFullName -> _)
    case tpd.Apply(function, arguments) if qualifiedName(function.symbol).exists(Named) =>
      constantName(arguments).flatMap(name => JdkPackages.unsupportedName(name).map(name -> _))
    case _ => None
  }

  /** The member a lookup of a class names, with the class it is in and the reason the class is unsupported. */
  private def lookup(function: tpd.Tree, arguments: List[tpd.Tree])(using Context): Option[(String, String, String)] =
    function match {
      case tpd.Select(qualifier, _)
          if function.symbol.owner.fullName.toString == "java.lang.Class" &&
            Lookups(function.symbol.name.toString) =>
        receiver(qualifier).map((owner, why) =>
          (constantName(arguments).getOrElse(function.symbol.name.toString), owner, why)
        )
      case tpd.Select(_, _)
          if function.symbol.owner.fullName.toString == "java.lang.invoke.MethodHandles.Lookup" &&
            function.symbol.name.toString.startsWith("find") && function.symbol.name.toString != "findClass" =>
        arguments.headOption.flatMap(argument => receiver(argument)).map((owner, why) =>
          (arguments.lift(1).flatMap(constantOf).getOrElse(function.symbol.name.toString), owner, why)
        )
      case _ => None
    }

  private def constantOf(tree: tpd.Tree): Option[String] = tree match {
    case tpd.Literal(Constant(name: String)) => Some(name)
    case _                                   => None
  }

  private def report(message: String, tree: tpd.Tree)(using Context): Unit =
    checks(Check.JdkInternal, s"$message; $Advice", tree.srcPos)
}
