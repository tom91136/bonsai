package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import java.util.jar.JarFile
import scala.util.Using

/** The artifacts a classpath names. */
private[analysis] object Classpath {

  private given Ordering[Vector[BigInt]] = Ordering.Implicits.seqOrdering[Vector, BigInt]

  /** Keeps the newest version of each repository artifact, as dependency resolution would for a single classpath. */
  def evicted(classpath: Vector[Path]): Vector[Path] = {
    val versioned = classpath.flatMap(path => artifact(path).map(path -> _)).toMap
    val newest    = versioned.groupBy(_._2._1).values.map(_.maxBy(_._2._2)._1).toSet
    classpath.filter(path => !versioned.contains(path) || newest(path))
  }

  /** The identity and version of a jar, from its manifest or else its Maven-style repository layout. */
  private def artifact(path: Path): Option[(String, (Vector[BigInt], String))] =
    manifested(path).orElse(laidOut(path)).map((identity, version) =>
      identity ->
        (version.split("[.-]").view.map(_.takeWhile(_.isDigit)).takeWhile(_.nonEmpty).map(BigInt(_)).toVector, version)
    )

  private def manifested(path: Path): Option[(String, String)] =
    if !path.getFileName.toString.endsWith(".jar") || !Files.isRegularFile(path) then None
    else
      Using.resource(JarFile(path.toFile)) { jar =>
        for {
          attributes <- Option(jar.getManifest).map(_.getMainAttributes)
          vendor     <- Option(attributes.getValue("Implementation-Vendor-Id"))
          title      <- Option(attributes.getValue("Implementation-Title"))
          version    <- Option(attributes.getValue("Implementation-Version"))
        } yield s"$vendor:$title" -> version
      }

  private def laidOut(path: Path): Option[(String, String)] = for {
    versionDirectory  <- Option(path.getParent)
    artifactDirectory <- Option(versionDirectory.getParent)
    version = versionDirectory.getFileName.toString
    if path.getFileName.toString == s"${artifactDirectory.getFileName}-$version.jar"
  } yield artifactDirectory.toString -> version
}
