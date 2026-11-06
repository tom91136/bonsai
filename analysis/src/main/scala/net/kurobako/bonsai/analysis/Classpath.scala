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
