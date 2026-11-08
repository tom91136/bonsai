package net.kurobako.bonsai.plugin

import dotty.tools.dotc.plugins.BonsaiLoaders
import java.io.DataInputStream
import java.lang.invoke.MethodHandles
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.AbstractMap.SimpleImmutableEntry
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.ZipFile
import scala.util.Using

/** The loaders that run the plugin, one for each compiler and plugin jar rather than one for each compilation.
  *
  * A compiler loads a plugin jar into a loader of its own for every run, and a build tool makes a run for every project
  * and again for every change, so the classes of the plugin would be loaded, verified and compiled again each time. The
  * loader here is the same for all of them, as long as the compiler and the jar are.
  */
private object Loaders {
  private type Table = ConcurrentHashMap[String, java.util.Map.Entry[String, ClassLoader]]

  private val Holder = classOf[BonsaiLoaders.type].getName

  /** The loader to run the plugin of `entry` with. It is shared where the plugin is a jar, and is that of `entry` where
    * the plugin is a directory, whose stamp says nothing of the files in it.
    */
  def of(entry: Class[?], compiler: Class[?]): ClassLoader =
    Option(entry.getProtectionDomain.getCodeSource).map(source => Paths.get(source.getLocation.toURI)) match {
      case Some(jar) if Files.isRegularFile(jar) => shared(compiler, jar)
      case _                                     => entry.getClassLoader
    }

  /** The loader of the classes of `jar`, whose parent is the loader of `compiler`: the one loader made for this
    * compiler and this state of the jar. A jar rewritten in place gets a loader of its own and the old one is dropped.
    */
  def shared(compiler: Class[?], jar: Path): ClassLoader = {
    val stamp = s"${Files.size(jar)}/${Files.getLastModifiedTime(jar).toMillis}"
    table(compiler, jar)
      .compute(
        jar.toString,
        (_, known) =>
          Option(known).filter(_.getKey == stamp).getOrElse(
            SimpleImmutableEntry(stamp, URLClassLoader(Array(jar.toUri.toURL), compiler.getClassLoader))
          )
      )
      .getValue
  }

  // The jar defines the table in the loader of the compiler unless a plugin of the same loader has.
  private def table(
      compiler: Class[?],
      jar: Path
  ): Table = {
    val loader = compiler.getClassLoader
    val holder =
      try Class.forName(Holder, false, loader)
      catch {
        case _: ClassNotFoundException =>
          try defined(compiler, jar)
          catch { case _: LinkageError => Class.forName(Holder, false, loader) }
      }
    holder.getField("MODULE$").get(null).asInstanceOf[Table] // scalafix:ok DisableSyntax.null
  }

  private def defined(compiler: Class[?], jar: Path): Class[?] = {
    val bytes = Using.resource(ZipFile(jar.toFile)) { archive =>
      val entry   = archive.getEntry(Holder.replace('.', '/') + ".class")
      val content = Array.ofDim[Byte](entry.getSize.toInt)
      Using.resource(archive.getInputStream(entry))(DataInputStream(_).readFully(content))
      content
    }
    if (JdkPackages.modular) MethodHandles.privateLookupIn(compiler, MethodHandles.lookup()).defineClass(bytes)
    else {
      val define = classOf[ClassLoader]
        .getDeclaredMethod("defineClass", classOf[String], classOf[Array[Byte]], classOf[Int], classOf[Int])
      define.setAccessible(true)
      define.invoke(compiler.getClassLoader, Holder, bytes, 0, bytes.length).asInstanceOf[Class[?]]
    }
  }
}
