package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.util.jar.JarFile
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.Using

/** Names the compiler cannot hold together: it enters every top-level class beside an object of its name, and refuses a
  * package of that name in the same package, so a build where one module has the class and an unrelated one the package
  * is unreadable in a single run. Such a build is divided into runs of modules that hold no such pair, each closed
  * under the modules that depend on its members.
  */
private[analysis] object Clashes {

  private case class Holding(classes: Set[String], packages: Set[String]) {
    def ++(other: Holding): Holding = Holding(classes ++ other.classes, packages ++ other.packages)
  }

  private case class Clash(name: String, classed: String, packaged: String, tail: String = "") {
    def shown: String =
      s"$name is a class in $classed and a package in $packaged, which the compiler cannot read together$tail"
  }

  private case class Pair(classed: String, packaged: String, name: String)

  private val Compiled = Set(".class", ".tasty")

  private def holding(files: Vector[Vector[String]]): Holding = {
    val packages = files.flatMap(_.init.scanLeft(Vector.empty[String])(_ :+ _).tail).map(_.mkString(".")).toSet
    val classes  = files.flatMap { path =>
      val name = path.last.takeWhile(_ != '.').stripSuffix("$")
      Option.when(name.nonEmpty)((path.init :+ name).mkString("."))
    }.toSet
    Holding(classes, packages)
  }

  private def held(output: Output, files: Vector[Path]): Holding = holding(
    files.map(output.path.relativize(_)).filter(file => Compiled.exists(file.getFileName.toString.endsWith(_)))
      .map(_.asScala.map(_.toString).toVector)
  )

  private def held(jar: Path): Holding = holding(
    Try(Using.resource(JarFile(jar.toFile))(
      _.entries.asScala.map(_.getName).filter(name => Compiled.exists(name.endsWith(_))).map(_.split('/').toVector)
        .toVector
    )).getOrElse(Vector.empty)
  )

  private def clashing(one: Holding, other: Holding): Vector[String] = (one.classes & other.packages).toVector.sorted

  private def shown(pair: Pair, tail: String): String =
    Clash(pair.name, s"module ${pair.classed}", s"module ${pair.packaged}", tail).shown

  /** The sets of modules to read in runs of their own, the first being the bulk of the build: where a class of one
    * module is a package of another, the smaller of the two sides, a module with those that depend on it, is read
    * apart. Fails naming the clash where a module needs both sides, where a run holds a class and a package of a
    * library on its classpath, or where a platform twin would be read apart.
    */
  def runs(manifest: Manifest, outputs: Vector[(Output, Vector[Path])]): Either[Vector[String], Vector[Set[Output]]] =
    modules(manifest, outputs).map(_.map(group =>
      outputs.collect { case (output, _) if group(output.module) => output }.toSet
    ))

  type Division = (Manifest, Vector[(Output, Vector[Path])]) => Either[Vector[String], Vector[Set[Output]]]

  private def modules(
      manifest: Manifest,
      outputs: Vector[(Output, Vector[Path])]
  ): Either[Vector[String], Vector[Set[String]]] = {
    val holdings   = outputs.groupMapReduce(_._1.module)(held(_, _))(_ ++ _)
    val packagedIn = holdings.toVector.flatMap((module, held) => held.packages.map(_ -> module)).groupMap(_._1)(_._2)
    def clashes(modules: Set[String]) = (for {
      classed  <- modules.toVector
      name     <- holdings(classed).classes.toVector
      packaged <- packagedIn.getOrElse(name, Vector.empty) if modules(packaged)
    } yield Pair(classed, packaged, name)).sortBy(pair => (pair.classed, pair.packaged, pair.name))
    val everything = holdings.keySet
    if (clashes(everything).isEmpty) Right(Vector(everything))
    else {
      val owners = outputs.map((output, _) => output.path -> output.module).toMap
      val uses   = everything.map(module =>
        module -> manifest.classpath.getOrElse(module, Vector.empty).flatMap(owners.get).toSet.excl(module)
      ).toMap
      def needs(module: String, seen: Set[String] = Set.empty): Set[String] =
        uses(module).diff(seen).foldLeft(seen ++ uses(module))((all, next) => needs(next, all))
      def visible(modules: Set[String]): Set[String] = modules ++ modules.flatMap(needs(_))
      val needing    = everything.toVector.flatMap(module => needs(module).map(_ -> module)).groupMap(_._1)(_._2)
      val dependents =
        everything.map(module => module -> (needing.getOrElse(module, Vector.empty).toSet + module)).toMap
      val sizes = outputs.groupMapReduce(_._1.module)(_._2.size)(_ + _)
      def divide(modules: Set[String]): Either[Vector[String], Vector[Set[String]]] =
        clashes(visible(modules)).headOption match {
          case None        => Right(Vector(modules))
          case Some(clash) =>
            val classed  = dependents(clash.classed) & modules
            val packaged = dependents(clash.packaged) & modules
            val both     = classed & packaged
            if (both.nonEmpty) Left(Vector(shown(clash, s", and ${both.toVector.sorted.mkString(", ")} need both")))
            else {
              val apart = if (classed.toVector.map(sizes).sum <= packaged.toVector.map(sizes).sum) classed else packaged
              for {
                rest  <- divide(modules -- apart)
                extra <- divide(apart)
              } yield rest ++ extra
            }
        }
      divide(everything).flatMap { divided =>
        val twinned = divided.drop(1).flatMap(_.filter(manifest.platforms)).sorted
        val tail    = s", and ${twinned.mkString(", ")} compile for another platform"
        val jarred  = divided.flatMap { modules =>
          val sorted = modules.toVector.sorted
          for {
            jar <- manifest.classpathOf(sorted).filter(Files.isRegularFile(_))
            library = held(jar)
            name    = s"library ${jar.getFileName}"
            module <- sorted
            clash  <- clashing(holdings(module), library).map(Clash(_, s"module $module", name)) ++
              clashing(library, holdings(module)).map(Clash(_, name, s"module $module"))
          } yield clash
        }
        if (twinned.nonEmpty)
          Left(clashes(everything).map(shown(_, tail)))
        else if (jarred.nonEmpty) Left(jarred.map(_.shown))
        else Right(divided)
      }
    }
  }
}
