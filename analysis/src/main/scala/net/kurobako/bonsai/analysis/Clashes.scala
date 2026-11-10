package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.util.jar.JarFile
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.Using

/** Names the compiler cannot hold together: it enters every top-level class beside an object of its name, and refuses a
  * package of that name in the same package, so a build where one module or library has the class and an unrelated one
  * the package is unreadable in a single run. The modules' own outputs are searched for such pairs before the read, as
  * the read completes all of them; a library is searched only for the names the compiler reports, as it completes the
  * packages it needs alone. Such a build is divided into runs of modules that hold no such pair, each closed under the
  * modules that depend on its members.
  */
private[analysis] object Clashes {

  private case class Holding(classes: Set[String], packages: Set[String]) {
    def ++(other: Holding): Holding = Holding(classes ++ other.classes, packages ++ other.packages)
  }

  private sealed trait Holder {
    def shown: String
  }

  private case class Owned(module: String) extends Holder {
    def shown: String = s"module $module"
  }

  private case class Library(path: Path) extends Holder {
    def shown: String = s"library $path"
  }

  private case class Pair(classed: Holder, packaged: Holder, name: String)

  private val Compiled = Set(".class", ".tasty")

  private val Reported = raw"(\S+) contains object and package with same name: (\S+)".r

  /** The qualified names of the classes and packages that the compiler reports in `messages` as clashing. */
  def reported(messages: Seq[String]): Set[String] = messages.flatMap(Reported.findAllMatchIn(_).map { found =>
    if (found.group(1).startsWith("<")) found.group(2) else s"${found.group(1)}.${found.group(2)}"
  }).toSet

  def unresolved(names: Set[String]): Vector[String] = names.toVector.sorted.map(name =>
    s"$name is a class and a package on the classpath, which the compiler cannot read together"
  )

  private def mayBePackage(segments: Vector[String]): Boolean =
    segments.forall(segment => segment.nonEmpty && segment != "META-INF" && !segment.startsWith("."))

  private def tally(files: Vector[Vector[String]], directories: Vector[Vector[String]]): Holding = {
    val compiled = files.filter(file => mayBePackage(file.init) && Compiled.exists(file.last.endsWith(_)))
    val existing = (files.map(_.init) ++ directories).filter(mayBePackage(_)).distinct
    val parents  = existing.flatMap(path => path.indices.drop(1).map(path.take))
    val packages = (compiled.map(_.init) ++ parents).filter(_.nonEmpty).map(_.mkString(".")).toSet
    val classes  = compiled.flatMap { file =>
      val name = file.last.takeWhile(_ != '.').stripSuffix("$")
      Option.when(file.init.nonEmpty && name.nonEmpty)((file.init :+ name).mkString("."))
    }.toSet
    Holding(classes, packages)
  }

  private def held(path: Path): Holding = Try {
    def segments(name: String) = name.split('/').toVector
    if (Files.isDirectory(path)) {
      val (directories, files) = Using.resource(Files.walk(path))(_.toList.asScala.toVector).filter(_ != path)
        .partition(Files.isDirectory(_))
      def relative(entry: Path) = path.relativize(entry).asScala.map(_.toString).toVector
      tally(files.map(relative(_)), directories.map(relative(_)))
    } else
      Using.resource(JarFile(path.toFile)) { jar =>
        val (directories, files) = jar.entries.asScala.map(_.getName).toVector.partition(_.endsWith("/"))
        tally(files.map(segments(_)), directories.map(segments(_)))
      }
  }.getOrElse(Holding(Set.empty, Set.empty))

  private def shown(pair: Pair, tail: String): String =
    s"${pair.name} is a class in ${pair.classed.shown} and a package in ${pair.packaged.shown}, " +
      s"which the compiler cannot read together$tail"

  /** The sets of modules to read in runs of their own, the first being the bulk of the build: where a class of one
    * module or library is a package of another, the smaller of the two sides, the modules that hold or use it with
    * those that depend on them, is read apart. `known` names the clashes the compiler has reported. Fails naming the
    * clash where a module needs both sides, or where a platform twin would be read apart.
    */
  def runs(
      manifest: Manifest,
      outputs: Vector[(Output, Vector[Path])],
      known: Set[String]
  ): Either[Vector[String], Vector[Set[Output]]] =
    modules(manifest, outputs, known).map(_.map(group =>
      outputs.collect { case (output, _) if group(output.module) => output }.toSet
    ))

  type Division =
    (Manifest, Vector[(Output, Vector[Path])], Set[String]) => Either[Vector[String], Vector[Set[Output]]]

  private def modules(
      manifest: Manifest,
      outputs: Vector[(Output, Vector[Path])],
      known: Set[String]
  ): Either[Vector[String], Vector[Set[String]]] = {
    val holdings   = outputs.groupMapReduce(_._1.module)((output, _) => held(output.path))(_ ++ _)
    val fixed      = manifest.classpath.keySet -- holdings.keySet
    val everything = holdings.keySet ++ fixed
    val owners     = outputs.map((output, _) => output.path -> output.module).toMap
    val libraries  = mutable.Map.empty[Path, Holding]
    def clashes(owned: Set[String], running: Set[String]): Vector[Pair] = {
      val modular: Vector[(Holder, Holding)] =
        owned.toVector.sorted.flatMap(module => holdings.get(module).map(Owned(module) -> _))
      val names =
        (modular.flatMap(_._2.classes).toSet.intersect(modular.flatMap(_._2.packages).toSet) ++ known).toVector
      if (names.isEmpty) Vector.empty
      else {
        val jars = Classpath.evicted(manifest.classpathOf(running.toVector.sorted)).filterNot(owners.contains)
        jars.foreach(jar => libraries.getOrElseUpdate(jar, held(jar)))
        val holders    = modular ++ jars.map(jar => Library(jar) -> libraries(jar))
        val classed    = holders.flatMap((holder, contents) => names.filter(contents.classes).map(_ -> holder))
        val packaged   = holders.flatMap((holder, contents) => names.filter(contents.packages).map(_ -> holder))
        val packagedBy = packaged.groupMap(_._1)(_._2)
        classed.flatMap((name, holder) => packagedBy.getOrElse(name, Vector.empty).map(Pair(holder, _, name)))
          .sortBy(pair => (pair.classed.shown, pair.packaged.shown, pair.name))
      }
    }
    val whole = clashes(holdings.keySet, everything)
    if (whole.isEmpty) Right(Vector(everything))
    else {
      val uses = everything.map(module =>
        module -> manifest.classpath.getOrElse(module, Vector.empty).flatMap(owners.get).toSet.excl(module)
      ).toMap
      def needs(module: String, seen: Set[String] = Set.empty): Set[String] =
        uses(module).diff(seen).foldLeft(seen ++ uses(module))((all, next) => needs(next, all))
      def visible(modules: Set[String]): Set[String] = modules ++ modules.flatMap(needs(_))
      val needing    = everything.toVector.flatMap(module => needs(module).map(_ -> module)).groupMap(_._1)(_._2)
      val dependents =
        everything.map(module => module -> (needing.getOrElse(module, Vector.empty).toSet + module)).toMap
      def holding(holder: Holder, modules: Set[String]): Set[String] = holder match {
        case Owned(module) => dependents(module) & modules
        case Library(path) => modules.filter(manifest.classpath.getOrElse(_, Vector.empty).contains(path))
      }
      def movable(side: Set[String], modules: Set[String]): Boolean =
        !side.exists(fixed) && (modules -- side).exists(holdings.contains)
      val sizes = outputs.groupMapReduce(_._1.module)(_._2.size)(_ + _)
      def divide(modules: Set[String]): Either[Vector[String], Vector[Set[String]]] =
        clashes(visible(modules), modules).headOption match {
          case None        => Right(Vector(modules))
          case Some(clash) =>
            val classed  = holding(clash.classed, modules)
            val packaged = holding(clash.packaged, modules)
            val both     = classed & packaged
            if (both.nonEmpty) Left(Vector(shown(clash, s", and ${both.toVector.sorted.mkString(", ")} need both")))
            else
              Vector(classed, packaged).filter(movable(_, modules)).minByOption(_.toVector.map(sizes).sum) match {
                case None =>
                  val stuck = (classed ++ packaged).toVector.sorted.mkString(", ")
                  Left(Vector(shown(clash, s", and $stuck cannot be read apart")))
                case Some(apart) =>
                  for {
                    rest  <- divide(modules -- apart)
                    extra <- divide(apart)
                  } yield rest ++ extra
              }
        }
      divide(everything).flatMap { divided =>
        val twinned = twins(manifest, outputs, divided)
        val tail    = s", and ${twinned.mkString(", ")} compile for another platform"
        if (twinned.nonEmpty) Left(whole.map(shown(_, tail))) else Right(divided)
      }
    }
  }

  private def twins(
      manifest: Manifest,
      outputs: Vector[(Output, Vector[Path])],
      divided: Vector[Set[String]]
  ): Vector[String] = {
    val compiled = outputs.groupMapReduce(_._1.module)((output, files) =>
      files.map(output.path.relativize(_)).filter(_.getFileName.toString.endsWith(".tasty")).toSet
    )(_ ++ _)
    val groupOf              = divided.zipWithIndex.flatMap((group, index) => group.map(_ -> index)).toMap
    def parted(twin: String) = compiled.keySet.exists(peer =>
      !manifest.platforms(peer) && groupOf(peer) != groupOf(twin) && compiled(peer).exists(compiled(twin))
    )
    divided.zipWithIndex.flatMap((group, index) =>
      group.filter(twin => manifest.platforms(twin) && (index > 0 || parted(twin)))
    ).sorted
  }
}
