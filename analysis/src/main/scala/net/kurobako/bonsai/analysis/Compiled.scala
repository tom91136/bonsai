package net.kurobako.bonsai.analysis

import java.nio.file.Files
import java.nio.file.Path
import net.kurobako.bonsai.collections.*

/** The findings the compiler plugin kept beside the classes of the modules it compiled in the audit mode.
  *
  * A record lives and goes with the classes of its source, so a cached build restores it and a compile that leaves a
  * source alone leaves its record. A record whose source is gone, or that the module no longer compiles, is not read.
  * Every source of Scala a recorded module compiles must have a record made from the contents it has now, as a report
  * with some sources missing would pass for a complete one.
  */
private[analysis] object Compiled {
  private val Listed = 5

  private val Remedy = "the module was not compiled in the audit mode, or a failed or cached compile left the " +
    "findings out of step with the sources, so run clean and compile again"

  def findings(manifest: Manifest): Either[Vector[String], Vector[Finding]] =
    manifest.root match {
      case None if manifest.recorded.nonEmpty =>
        Left(Vector("the manifest names no root for the findings the compiler plugin recorded"))
      case root =>
        val outputs = manifest.outputs.filter(output => manifest.recorded(output.module))
          .groupMap(output => (output.module, output.scope))(_.path)
        val sources = manifest.sources.groupMap(source => (source.module, source.scope))(_.path)
        val read    = root.toVector.flatMap(base =>
          outputs.toVector.sortBy((key, _) => (key._1, key._2.ordinal)).map { case ((module, scope), directories) =>
            scoped(manifest, base, module, scope, directories, sources.getOrElse((module, scope), Vector.empty))
          }
        )
        val problems = read.flatMap(_.left.toOption).flatten
        if (problems.nonEmpty) Left(problems) else Right(read.flatMap(_.toOption).flatten)
    }

  private def scoped(
      manifest: Manifest,
      root: Path,
      module: String,
      scope: Output.Scope,
      directories: Vector[Path],
      sources: Vector[Path]
  ): Either[Vector[String], Vector[Finding]] = {
    val files      = directories.flatMap(Recorded.all)
    val unreadable = files.collect { case (file, None) =>
      s"$module has a record of compiler findings that cannot be read: $file"
    }
    val records  = files.flatMap(_._2).groupBy(_.source)
    val expected = sources.filter(_.toString.endsWith(".scala")).map(path => Recorded.key(root, path) -> path).toMap
    val absent   = expected.view.filterKeys(!records.contains(_)).keys.toVector.sorted
    val present  = (expected ++ generated(manifest, root, records.keys.filterNot(expected.contains))).toVector.sortBy(
      _._1
    )
    val digests = present.collect { case (key, path) if Files.isRegularFile(path) => key -> digest(path) }.toMap
    val stale   = present.collect {
      case (key, _) if records.contains(key) && digests.get(key).exists(now => !records(key).exists(_.digest == now)) =>
        key
    }
    val problems = unreadable ++ describe(
      module,
      "has no recorded compiler findings for",
      absent.filter(key => Files.isRegularFile(expected(key)))
    ) ++
      describe(module, "has compiler findings recorded for an older version of", stale)
    if (problems.nonEmpty) Left(problems)
    else {
      val roots = manifest.generated.map(_.toAbsolutePath.normalize).toSet
      Right(present.flatMap((key, path) =>
        digests.get(key).toVector.flatMap(now =>
          records.getOrElse(key, Vector.empty).filter(_.digest == now).flatMap(_.entries).distinct.map(entry =>
            Finding(
              Finding.Rule.compiled(entry.rule),
              Node(key, module, scope, key, entry.line, within(path, roots), exported = false, outer = None),
              entry.message,
              level(entry.level)
            )
          )
        )
      ))
    }
  }

  private def generated(manifest: Manifest, root: Path, keys: Iterable[String]): Map[String, Path] = {
    val roots = manifest.generated.map(_.toAbsolutePath.normalize).toSet
    keys.map(key => key -> root.resolve(key)).filter((_, path) =>
      Files.isRegularFile(path) && within(path, roots)
    ).toMap
  }

  // a hash lookup for each directory above the path, whatever the number of generated paths
  private def within(path: Path, roots: Set[Path]): Boolean =
    Iterator.iterate(Option(path.toAbsolutePath.normalize))(_.flatMap(found => Option(found.getParent)))
      .takeWhile(_.isDefined).flatten.exists(roots)

  private def digest(path: Path): String = Recorded.digest(Files.readAllBytes(path))

  private def level(recorded: Recorded.Level): Finding.Level = recorded match {
    case Recorded.Level.Error   => Finding.Level.Error
    case Recorded.Level.Warning => Finding.Level.Warning
    case Recorded.Level.Info    => Finding.Level.Info
  }

  private def describe(module: String, what: String, keys: Vector[String]): Vector[String] =
    if (keys.isEmpty) Vector.empty
    else
      Vector(
        s"$module $what ${Finding.listed(keys, Listed)}; $Remedy"
      )
}
