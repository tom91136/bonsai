package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.util.jar.JarFile
import scala.util.Try
import scala.util.Using

/** The artifacts a classpath names. */
private[analysis] object Classpath {

  private given Ordering[Vector[BigInt]] = Ordering.Implicits.seqOrdering[Vector, BigInt]

  /** Keeps the first version of each artifact, at the positions its copies held. */
  def shadowed(classpath: Vector[Path]): Vector[Path] = choosing(classpath, Map.empty)

  private def choosing(classpath: Vector[Path], chosen: Map[String, Vector[BigInt]]): Vector[Path] = {
    val versioned = classpath.flatMap(path => artifact(path).map(path -> _)).toMap
    val taking    = classpath.flatMap(versioned.get(_)).foldLeft(chosen) { case (seen, (identity, version)) =>
      seen.updatedWith(identity)(_.orElse(Some(version)))
    }
    classpath.filter(path => versioned.get(path).forall((identity, version) => taking(identity) == version))
  }

  /** The version of each artifact that a compile on `classpath` takes, the first, with its jar, the standard library
    * aside, which every read replaces with the one the analysis runs on.
    */
  def taken(classpath: Vector[Path]): Map[String, (Vector[BigInt], Path)] =
    classpath.filter(kind(_).isEmpty).flatMap(path =>
      artifact(path).map((identity, version) => identity -> (version, path))
    )
      .foldLeft(Map.empty[String, (Vector[BigInt], Path)]) { case (seen, (identity, entry)) =>
        seen.updatedWith(identity)(_.orElse(Some(entry)))
      }

  /** The newest version of each artifact among those that compiles take. */
  def newest(takes: Iterable[Map[String, (Vector[BigInt], Path)]]): Map[String, Vector[BigInt]] =
    takes.flatMap(_.view.mapValues(_._1)).groupMapReduce(_._1)(_._2)(Ordering[Vector[BigInt]].max)

  /** The entries of `classpath` the compiler reads where of each artifact `chosen` names it takes that version, the
    * first otherwise, against the standard library of the analysis.
    */
  def keeping(classpath: Vector[Path], chosen: Map[String, Vector[BigInt]]): Vector[Path] =
    unified(choosing(classpath, chosen))

  /** `classpath` with the JVM standard library jars of the build, whatever their version, replaced by the one the
    * analysis runs on at the position of the first, as the compiler that reads the build's TASTy expects it. The jars
    * of a platform's variant of the library, which hold classes the JVM one lacks, follow it. A jar is a standard
    * library by its name and by holding the `Predef` of the library.
    */
  def unified(classpath: Vector[Path]): Vector[Path] = running.fold(classpath) { library =>
    val kinds = classpath.map(kind(_))
    val first = kinds.indexWhere(_.nonEmpty)
    if (first < 0) classpath
    else {
      val variants = classpath.zip(kinds).collect { case (path, Some(Library.Variant)) => path }
      classpath.zip(kinds).zipWithIndex.flatMap {
        case (_, index) if index == first => library +: variants
        case ((path, None), _)            => Vector(path)
        case _                            => Vector.empty
      }
    }
  }

  private enum Library { case Jvm, Variant }

  private def kind(path: Path): Option[Library] = {
    val name = path.getFileName.toString
    if (StandardLibrary.matches(name) && Markers.exists(holds(path.toString, _)))
      Some(if (JvmLibrary.matches(name)) Library.Jvm else Library.Variant)
    else None
  }

  private val Markers = Seq("scala/Predef.class", "scala/runtime/stdLibPatches/Predef.class")

  /** The entries of `classpath` the compiler reads: the first version of each artifact, against the standard library of
    * the analysis.
    */
  def readable(classpath: Vector[Path]): Vector[Path] = unified(shadowed(classpath))

  def scalaJs(classpath: Vector[Path]): Boolean =
    classpath.exists(path => holds(path.toString, "scala/scalajs/js/Any.class"))

  private val StandardLibrary = """scala3?-library.*\.jar""".r

  private val JvmLibrary = """scala-library-.*\.jar|scala3-library_3-.*\.jar""".r

  private val running: Option[Path] =
    Try(Path.of(classOf[Predef.type].getProtectionDomain.getCodeSource.getLocation.toURI)).toOption
      .filter(Files.isRegularFile(_))

  /** The first root holding `file`, a class file path as the compiler reports it, in the compiler's search order. */
  def holder(roots: Vector[String], file: String): Option[String] = roots.find(holds(_, file))

  private def holds(root: String, file: String): Boolean = {
    val directory = Path.of(root)
    if (Files.isDirectory(directory)) Path.of(file).isAbsolute && Path.of(file).startsWith(directory)
    else
      Files.isRegularFile(directory) &&
      Try(Using.resource(JarFile(directory.toFile))(jar => Option(jar.getEntry(file)).nonEmpty)).getOrElse(false)
  }

  /** The identity and numeric version of a jar, from its manifest or else its Maven-style repository layout. */
  private def artifact(path: Path): Option[(String, Vector[BigInt])] =
    manifested(path).orElse(laidOut(path)).map((identity, version) =>
      identity -> version.split("[.-]").view.map(_.takeWhile(_.isDigit)).takeWhile(_.nonEmpty).map(BigInt(_)).toVector
    )

  private def manifested(path: Path): Option[(String, String)] =
    if (!path.getFileName.toString.endsWith(".jar") || !Files.isRegularFile(path)) None
    else
      Using.resource(JarFile(path.toFile)) { jar =>
        for {
          attributes <- Option(jar.getManifest).map(_.getMainAttributes)
          vendor     <- Option(attributes.getValue("Implementation-Vendor-Id"))
          title      <- Option(attributes.getValue("Implementation-Title"))
          version    <- Option(attributes.getValue("Implementation-Version"))
        } yield s"$vendor:$title:${path.getFileName.toString.stripSuffix(".jar").stripSuffix(s"-$version")}" -> version
      }

  private def laidOut(path: Path): Option[(String, String)] = for {
    versionDirectory  <- Option(path.getParent)
    artifactDirectory <- Option(versionDirectory.getParent)
    version = versionDirectory.getFileName.toString
    if path.getFileName.toString == s"${artifactDirectory.getFileName}-$version.jar"
  } yield artifactDirectory.toString -> version
}
