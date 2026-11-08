package net.kurobako.bonsai.analysis

import java.io.File
import java.net.URI
import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Path
import java.util.jar.JarFile
import java.util.zip.ZipException
import java.util.zip.ZipFile
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Using

private[analysis] object ClassSource {

  private[analysis] case class Target(feature: Int, root: Path)

  private enum RootKind {
    case Directory, Archive, Absent
  }

  def of(outputs: Vector[Path], classpath: Vector[Path], jdk: Option[Path]): Either[String, ClassSource] = {
    val target = jdk.fold[Either[String, Option[Target]]](Right(None))(home => targetOf(home).map(Some(_)))
    target.map(ClassSource(_, outputs, Classpath.evicted(classpath)))
  }

  private def targetOf(home: Path): Either[String, Target] = {
    val release = home.resolve("release")
    featureOf(release).toRight(s"$release: no JAVA_VERSION").flatMap { feature =>
      if (feature >= 9) Either.cond(isImage(home), Target(feature, home), s"$home: no lib/modules")
      else rtJar(home).map(Target(feature, _)).toRight(s"$home: no rt.jar")
    }
  }

  private def featureOf(release: Path): Option[Int] = {
    val lines = if (Files.isRegularFile(release)) Files.readAllLines(release).asScala else Seq.empty
    lines.collectFirst { case s"JAVA_VERSION=\"$version\"" => version }.flatMap {
      case s"1.$minor.$_" => minor.toIntOption
      case version        => version.takeWhile(_.isDigit).toIntOption
    }
  }

  private def isImage(home: Path): Boolean = Files.isRegularFile(home.resolve("lib/modules"))

  private def rtJar(home: Path): Option[Path] =
    Seq("jre/lib/rt.jar", "lib/rt.jar").map(home.resolve(_)).find(Files.isRegularFile(_))
}

private[analysis] class ClassSource private (
    jdk: Option[ClassSource.Target],
    outputs: Vector[Path],
    classpath: Vector[Path]
) extends AutoCloseable {

  private val images   = mutable.Map.empty[Path, FileSystem]
  private val jars     = mutable.Map.empty[Path, Option[JarFile]]
  private val packages = mutable.Map.empty[Path, Set[String]]
  private val kinds    = mutable.Map.empty[Path, ClassSource.RootKind]
  private val listed   = mutable.Map.empty[Path, Set[String]]

  def bytes(internal: String): Option[Array[Byte]] = {
    val entry = s"$internal.class"
    jdk.flatMap(fromJdk(_, internal))
      .orElse(outputs.view.flatMap(fromRoot(_, entry)).headOption)
      .orElse(classpath.view.flatMap(fromRoot(_, entry)).headOption)
  }

  override def close(): Unit = {
    jars.values.flatten.foreach(_.close())
    images.values.foreach(_.close())
  }

  private def fromJdk(target: ClassSource.Target, internal: String): Option[Array[Byte]] =
    if (target.feature >= 9) jrt(image(target.root), internal)
    else fromRoot(target.root, s"$internal.class")

  private def image(home: Path): FileSystem = images.getOrElseUpdate(
    home,
    FileSystems.newFileSystem(URI.create("jrt:/"), java.util.Map.of("java.home", home.toString))
  )

  private def jrt(image: FileSystem, internal: String): Option[Array[Byte]] = {
    val slash = internal.lastIndexOf('/')
    val pkg   = image.getPath("/packages", internal.take(slash).replace('/', '.'))
    Option.when(slash > 0 && Files.isDirectory(pkg)) {
      Using.resource(Files.newDirectoryStream(pkg)) { modules =>
        modules.asScala.view
          .map(module => image.getPath("/modules", module.getFileName.toString, s"$internal.class"))
          .find(Files.isRegularFile(_))
      }
    }.flatten.map(Files.readAllBytes(_))
  }

  private def fromRoot(root: Path, entry: String): Option[Array[Byte]] =
    rootKind(root) match {
      case ClassSource.RootKind.Directory =>
        Option.when(listed.getOrElseUpdate(
          root,
          listing(root)
        ).contains(entry))(Files.readAllBytes(root.resolve(entry)))
      case ClassSource.RootKind.Archive =>
        jarOf(root).filter(jar => holds(root, jar, entry)).flatMap(jar =>
          Option(jar.getEntry(entry)).map(found => Using.resource(jar.getInputStream(found))(_.readAllBytes()))
        )
      case ClassSource.RootKind.Absent => None
    }

  private def rootKind(root: Path): ClassSource.RootKind = kinds.getOrElseUpdate(
    root,
    if (Files.isDirectory(root)) ClassSource.RootKind.Directory
    else if (Files.isRegularFile(root)) ClassSource.RootKind.Archive
    else ClassSource.RootKind.Absent
  )

  private def listing(directory: Path): Set[String] =
    Outputs.files(directory).map(file => directory.relativize(file).toString.replace(File.separatorChar, '/')).toSet

  private def holds(file: Path, jar: JarFile, entry: String): Boolean = packages.getOrElseUpdate(
    file,
    jar.entries.asScala.map(found => directoryOf(Versioned.replaceFirstIn(found.getName, ""))).toSet
  ).contains(directoryOf(entry))

  private val Versioned = "^META-INF/versions/[0-9]+/".r

  private def directoryOf(entry: String): String = entry.take(entry.lastIndexOf('/') + 1)

  private def jarOf(file: Path): Option[JarFile] = jars.getOrElseUpdate(
    file,
    try
      Some(jdk match {
        case Some(target) =>
          val version = Runtime.Version.parse(target.feature.toString) // scalafix:ok ForbiddenAPIs.forbidden-api
          JarFile(file.toFile, true, ZipFile.OPEN_READ, version)
        case None => JarFile(file.toFile, true)
      })
    catch { case _: ZipException => None }
  )
}
