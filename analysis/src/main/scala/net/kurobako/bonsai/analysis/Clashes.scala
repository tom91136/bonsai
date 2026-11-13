package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.util.jar.JarFile
import scala.annotation.tailrec
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
  *
  * A class that two modules which are not platform twins both define is held apart likewise: the compiler resolves it
  * to one copy, and a caller compiled against the other loses the calls it makes into it. Libraries are not searched
  * for such copies, as shaded and split ones are common and the compiler reads them alike. Where one module needs both
  * copies they are read together, as they cannot be read apart.
  */
private[analysis] object Clashes {

  private case class Holding(classes: Set[String], packages: Set[String]) {
    def ++(other: Holding): Holding = Holding(classes ++ other.classes, packages ++ other.packages)
  }

  private sealed trait Holder {
    def shown: String
  }

  private case class Owned(part: Output.Part) extends Holder {
    def shown: String = s"module ${label(part)}"
  }

  private case class Library(path: Path) extends Holder {
    def shown: String = s"library $path"
  }

  private case class Pair(classed: Holder, against: Holder, name: String, copied: Boolean = false)

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

  private def described(pair: Pair): String =
    if (pair.copied) s"${pair.name} is a class in ${pair.classed.shown} and in ${pair.against.shown}"
    else s"${pair.name} is a class in ${pair.classed.shown} and a package in ${pair.against.shown}"

  private def shown(pair: Pair, tail: String): String =
    s"${described(pair)}, which the compiler cannot read together$tail"

  private def classNamed(name: String): Boolean =
    name.substring(name.lastIndexOf('.') + 1).forall(Character.isJavaIdentifierPart(_))

  /** The sets of compiles, each a scope of a module, to read in runs of their own, the first being the bulk of the
    * build. Compiles whose classpaths hold different versions of an artifact are read apart, as no read can hold both,
    * except platform twins and the modules they share classes with, which are read together. Within these, where a
    * class of one compile or library is a package of another, or a class of one compile is a class of another that is
    * not its platform twin, the smaller of the two sides, the compiles that hold or use it with those that depend on
    * them and the platform twins that share classes with them, is read apart. `known` names the clashes the compiler
    * has reported. Fails naming the clash where a compile, or a twin and its peers, needs both sides of a class and a
    * package. `because` names each version and clash a run was set apart for, as the failure would.
    */
  def runs(
      manifest: Manifest,
      outputs: Vector[(Output, Vector[Path])],
      known: Set[String]
  ): Either[Vector[String], Divided] =
    parts(manifest, outputs, known).map((groups, because) =>
      Divided(
        groups.map(group => outputs.collect { case (output, _) if group(output.part) => output }.toSet),
        because
      )
    )

  case class Divided(runs: Vector[Set[Output]], because: Vector[String])

  type Division = (Manifest, Vector[(Output, Vector[Path])], Set[String]) => Either[Vector[String], Divided]

  private given Ordering[Output.Part] = Ordering.by(part => (part._1, part._2.ordinal))

  private def label(part: Output.Part): String = if (part._2 == Output.Scope.Main) part._1 else s"${part._1} tests"

  private def named(identity: String): String = identity.split("[/:]").last

  private type Versions = Map[String, Set[Vector[BigInt]]]

  /** The compiles in groups whose classpaths hold one version of each artifact, the compiles in `tied` kept together
    * and those without outputs, `fixed`, in the first, which the compiles that agree with the most others join first.
    */
  private def apartByVersion(
      readings: Readings,
      everything: Set[Output.Part],
      fixed: Set[Output.Part],
      tied: Map[Output.Part, Set[Output.Part]]
  ): (Vector[Set[Output.Part]], Vector[String]) = {
    val members = everything.toVector.sorted
    val perPart = members.zip(Classpath.versions(members.map(part => readings.of(Set(part))))).toMap
    def versionsOf(parts: Set[Output.Part]): Versions =
      parts.toVector.flatMap(perPart(_).toVector).groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    val nodes = Option.when(fixed.nonEmpty)(fixed).toVector ++
      members.filterNot(fixed).map(part => tied.getOrElse(part, Set.empty) + part).distinct
    val seen    = nodes.map(versionsOf(_))
    val holders = seen.zipWithIndex.flatMap((versions, node) =>
      versions.toVector.flatMap((identity, all) => all.toVector.map(version => (identity, version, node)))
    ).groupMap(_._1)(entry => entry._2 -> entry._3).view.mapValues(_.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap)
      .toMap
    val rivals = seen.zipWithIndex.map((versions, node) =>
      versions.toVector.flatMap((identity, all) =>
        holders(identity).collect { case (version, others) if all != Set(version) => others }.flatten
      ).toSet - node
    )
    val first = if (fixed.nonEmpty) Vector(0) else Vector.empty
    val order = nodes.indices.drop(first.size).sortBy(node => (rivals(node).size, nodes(node).min))
    val runs  = order.foldLeft(first.map(_ -> 0).toMap) { (runs, node) =>
      val taken = rivals(node).flatMap(runs.get)
      runs.updated(node, Iterator.from(0).find(!taken(_)).getOrElse(0))
    }
    val groups = runs.toVector.groupMap(_._2)(entry => nodes(entry._1)).toVector.sortBy(_._1).map(_._2.flatten.toSet)
    val across =
      groups.flatMap(group => versionsOf(group).toVector).groupMap(_._1)(_._2).view.mapValues(_.flatten.toSet)
    val because = across.toVector.filter(_._2.size > 1).sortBy(_._1).map { (identity, all) =>
      val shown = all.toVector.sorted(using Ordering.Implicits.seqOrdering[Vector, BigInt]).map(_.mkString("."))
      s"${named(identity)} is on versions ${shown.mkString(" and ")}"
    }
    (groups, because)
  }

  private def parts(
      manifest: Manifest,
      outputs: Vector[(Output, Vector[Path])],
      known: Set[String]
  ): Either[Vector[String], (Vector[Set[Output.Part]], Vector[String])] = {
    val readings   = Readings(manifest)
    val holdings   = outputs.groupMapReduce(_._1.part)((output, _) => held(output.path))(_ ++ _)
    val fixed      = manifest.unbuilt
    val everything = holdings.keySet ++ fixed
    val owners     = outputs.map((output, _) => output.path -> output.module).toMap
    val jvm        = outputs.view.map(_._1.module).filterNot(manifest.platforms).toSet
    val libraries  = mutable.Map.empty[Path, Holding]
    lazy val tasty = outputs.groupMapReduce(_._1.part)((output, files) =>
      files.map(output.path.relativize(_)).filter(_.getFileName.toString.endsWith(".tasty")).toSet
    )(_ ++ _)
    lazy val sharing = tasty.toVector.flatMap((part, paths) => paths.toVector.map(_ -> part)).groupMap(_._1)(_._2)
      .values.map(_.toSet).filter(_.exists(part => manifest.platforms(part._1))).toVector
    lazy val mates = sharing.flatMap(group => group.toVector.map(_ -> group)).groupMapReduce(_._1)(_._2)(_ ++ _)
    lazy val peers = mutable.Map.empty[String, Set[String]]
    def seeing(twin: String): Set[String] =
      peers.getOrElseUpdate(twin, Twins.peers(manifest, jvm, twin).toSet + twin)
    def copies(modular: Vector[(Holder, Holding)]): Vector[Pair] = {
      val named = modular.flatMap((holder, contents) => contents.classes.filter(classNamed(_)).map(_ -> holder))
      named.groupMap(_._1)(_._2).toVector.filter(_._2.size > 1).flatMap { (name, holders) =>
        holders.combinations(2).collect {
          case Vector(first @ Owned(one), second @ Owned(other)) if !mates.getOrElse(one, Set.empty)(other) =>
            Pair(first, second, name, copied = true)
        }
      }.sortBy(pair => (pair.classed.shown, pair.against.shown, pair.name))
    }
    def clashes(owned: Set[Output.Part], running: Set[Output.Part]): Vector[Pair] = {
      val parted                             = owned.toVector.sorted.flatMap(part => holdings.get(part).map(part -> _))
      val modular: Vector[(Holder, Holding)] = parted.map((part, contents) => Owned(part) -> contents)
      val names                              =
        (modular.flatMap(_._2.classes).toSet.intersect(modular.flatMap(_._2.packages).toSet) ++ known).toVector
      def mixed(seen: Vector[(Holder, Holding)], jars: Vector[Path]): Vector[Pair] = {
        jars.foreach(jar => libraries.getOrElseUpdate(jar, held(jar)))
        val holders    = seen ++ jars.map(jar => Library(jar) -> libraries(jar))
        val classed    = holders.flatMap((holder, contents) => names.filter(contents.classes).map(_ -> holder))
        val packaged   = holders.flatMap((holder, contents) => names.filter(contents.packages).map(_ -> holder))
        val packagedBy = packaged.groupMap(_._1)(_._2)
        classed.flatMap((name, holder) => packagedBy.getOrElse(name, Vector.empty).map(Pair(holder, _, name)))
      }
      val mixes =
        if (names.isEmpty) Vector.empty
        else {
          val read  = readings.of(running)
          val twins = running.view.map(_._1).filter(manifest.platforms).toVector.distinct.sorted
          def seen(visible: String => Boolean) = parted.collect {
            case (part, contents) if visible(part._1) => Owned(part) -> contents
          }
          mixed(seen(!manifest.platforms(_)), read.filterNot(owners.contains)) ++ twins.flatMap { twin =>
            mixed(
              seen(module => !manifest.platforms(module) || seeing(twin)(module)),
              readings.twin(twin, Vector.empty, read).filterNot(owners.contains)
            )
          }
        }
      mixes.distinct.sortBy(pair => (pair.classed.shown, pair.against.shown, pair.name)) ++ copies(modular)
    }
    val inside = everything.groupBy(_._1)
    val uses   = everything.map { part =>
      val modules = manifest.requires.getOrElse(part._1, Set.empty)
      part ->
        (modules.flatMap(inside.getOrElse(_, Set.empty)) ++
          Option.when(part._2 == Output.Scope.Test)(part._1 -> Output.Scope.Main).filter(everything))
    }.toMap
    def needs(part: Output.Part, seen: Set[Output.Part] = Set.empty): Set[Output.Part] =
      uses(part).diff(seen).foldLeft(seen ++ uses(part))((all, next) => needs(next, all))
    def visible(members: Set[Output.Part]): Set[Output.Part] = members ++ members.flatMap(needs(_))
    val needing = everything.toVector.flatMap(part => needs(part).map(_ -> part)).groupMap(_._1)(_._2)
    val near    = everything.map(part =>
      part -> (needing.getOrElse(part, Vector.empty).toSet ++ mates.getOrElse(part, Set.empty) + part)
    ).toMap
    @tailrec def following(members: Set[Output.Part]): Set[Output.Part] = {
      val next = members.flatMap(near)
      if (next.subsetOf(members)) members else following(members ++ next)
    }
    def holding(holder: Holder, members: Set[Output.Part]): Set[Output.Part] = holder match {
      case Owned(part)   => following(Set(part)) & members
      case Library(path) =>
        following(members.filter(part => readings.entries(Set(part)).contains(path))) & members
    }
    def movable(side: Set[Output.Part], members: Set[Output.Part]): Boolean =
      !side.exists(fixed) && (members -- side).exists(holdings.contains)
    def separable(clash: Pair, members: Set[Output.Part]): Boolean = {
      val classed = holding(clash.classed, members)
      val against = holding(clash.against, members)
      (classed & against).isEmpty && Vector(classed, against).exists(movable(_, members))
    }
    def listed(members: Set[Output.Part]) = members.toVector.sorted.map(label(_)).mkString(", ")
    val sizes                             = outputs.groupMapReduce(_._1.part)(_._2.size)(_ + _)
    val causes                            = mutable.LinkedHashSet.empty[String]
    def divide(members: Set[Output.Part]): Either[Vector[String], Vector[Set[Output.Part]]] =
      clashes(visible(members), members).find(clash => !clash.copied || separable(clash, members)) match {
        case None        => Right(Vector(members))
        case Some(clash) =>
          val classed  = holding(clash.classed, members)
          val packaged = holding(clash.against, members)
          val both     = classed & packaged
          if (both.nonEmpty) Left(Vector(shown(clash, s", and ${listed(both)} need both")))
          else
            Vector(classed, packaged).filter(movable(_, members)).minByOption(_.toVector.map(sizes).sum) match {
              case None => Left(Vector(shown(clash, s", and ${listed(classed ++ packaged)} cannot be read apart")))
              case Some(apart) =>
                causes += described(clash)
                for {
                  rest  <- divide(members -- apart)
                  extra <- divide(apart)
                } yield rest ++ extra
            }
      }
    val (versioned, differing) = apartByVersion(readings, everything, fixed, mates)
    versioned.foldLeft[Either[Vector[String], Vector[Set[Output.Part]]]](Right(Vector.empty))((all, group) =>
      for {
        done <- all
        next <- divide(group)
      } yield done ++ next
    ).map(_ -> (differing ++ causes))
  }
}
