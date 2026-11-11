package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.util.jar.JarFile
import scala.util.Try
import scala.util.Using

/** The artifacts a classpath names. */
private[analysis] object Classpath {

  private given Ordering[Vector[BigInt]] = Ordering.Implicits.seqOrdering[Vector, BigInt]

  /** Keeps the newest version of each artifact, at the position its first copy held. */
  def evicted(classpath: Vector[Path]): Vector[Path] = {
    val versioned = classpath.flatMap(path => artifact(path).map(path -> _)).toMap
    val newest    = versioned.groupMapReduce(_._2._1)(_._2._2)(Ordering[Vector[BigInt]].max)
    val copies    = classpath.filter(versioned.contains).groupBy(path => versioned(path)._1)
    val winners   =
      copies.map((identity, paths) => identity -> paths.filter(path => versioned(path)._2 == newest(identity)))
    classpath.flatMap(path =>
      versioned.get(path) match {
        case None                => Vector(path)
        case Some((identity, _)) => if (copies(identity).head == path) winners(identity) else Vector.empty
      }
    )
  }

  /** `classpath` with the standard library jars of the build, whatever their version and platform, replaced by the one
    * the analysis runs on at the position of the first, as the compiler that reads the build's TASTy expects it. A jar
    * is one by its name and by holding the `Predef` of the library.
    */
  def unified(classpath: Vector[Path]): Vector[Path] = running.fold(classpath) { library =>
    val standards = classpath.map(standard(_))
    val first     = standards.indexOf(true)
    if (first < 0) classpath
    else
      classpath.zip(standards).zipWithIndex.collect {
        case (_, index) if index == first => library
        case ((path, false), _)           => path
      }
  }

  private def standard(path: Path): Boolean =
    StandardLibrary.matches(path.getFileName.toString) && Markers.exists(holds(path.toString, _))

  private val Markers = Seq("scala/Predef.class", "scala/runtime/stdLibPatches/Predef.class")

  /** The entries of `classpath` the compiler reads: the newest of each artifact, against the standard library of the
    * analysis.
    */
  def readable(classpath: Vector[Path]): Vector[Path] = unified(evicted(classpath))

  private val StandardLibrary = """scala3?-library.*\.jar""".r

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
