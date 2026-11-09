package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path

/** The classes of platform twins that the run over the JVM modules' classes cannot read. */
private[analysis] object Twins {

  /** What a twin's own runs read: `own` are the classes the JVM modules lack, and `replaced` its copies of theirs that
    * differ, which the twin compiled against its own classes. A class identical to one the JVM modules, or an earlier
    * twin, hold is the same source compiled again, which their run covers; one named like a class the JVM modules hold
    * as Java, or that an earlier twin holds, stands for that class, so it is read for what it references.
    */
  case class Reading(module: String, replaced: Vector[Path], own: Vector[Path], declared: Set[String])

  /** The other twins that compile for the platform of `module`: those whose classpath holds a library no JVM module
    * has, as the library of the platform is. A twin's classes resolve against its peers' before the JVM modules'.
    */
  def peers(manifest: Manifest, jvm: Set[String], module: String): Vector[String] = {
    val platform = manifest.classpathOf(Seq(module)).toSet -- manifest.classpathOf(jvm.toSeq)
    manifest.platforms.toVector.filter(other =>
      other != module && manifest.classpathOf(Seq(other)).exists(platform)
    ).sorted
  }

  def reading(
      primary: Vector[(Output, Path)],
      twinned: Vector[(Output, Path)],
      compiled: Set[Path]
  ): Vector[Reading] = {
    def byPath(files: Vector[(Output, Path)]) = files.groupMap((output, file) => output.path.relativize(file))(_._2)
    def classFile(relative: Path)             =
      relative.resolveSibling(relative.getFileName.toString.stripSuffix(".tasty") + ".class")
    val (readings, _) = twinned.groupMap(_._1.module)(identity).toVector.sortBy(_._1).foldLeft(
      (Vector.empty[Reading], byPath(primary))
    ) { case ((done, held), (module, files)) =>
      def stands(output: Output, file: Path) =
        held.contains(output.path.relativize(file)) || compiled(classFile(output.path.relativize(file)))
      val (copies, own) = files.partition(stands(_, _))
      val differing     = copies.filter((output, file) =>
        !held.getOrElse(output.path.relativize(file), Vector.empty).exists(Files.mismatch(_, file) < 0)
      )
      val declared = differing.map((output, file) =>
        output.path.relativize(file).toString.stripSuffix(".tasty").replace('/', '.')
      ).toSet
      (done :+ Reading(module, differing.map(_._2), own.map(_._2), declared), Maps.join(held, byPath(own))(_ ++ _))
    }
    readings
  }
}
