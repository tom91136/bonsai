package net.kurobako.bonsai.sbt

import net.kurobako.bonsai.configuration.BonsaiConfiguration as Reader
import sbt.*
import sbt.internal.util.MessageOnlyException

/** `.bonsai.conf`, read by the reader the sbt plugin shares with the other tools, which fails the build with its
  * message.
  */
object BonsaiConfiguration {

  export Reader.Analysis
  export Reader.Configuration
  export Reader.DefaultBaseline
  export Reader.DefaultPurity
  export Reader.OptionFolds
  export Reader.Settings

  /** The glob of the sources a build generates into `directory`, relative to the `root` sources are named from. */
  def generatedGlob(root: File, directory: File): String =
    IO.relativize(root, directory).getOrElse(directory.getAbsolutePath) + "/**"

  def read(file: File): Configuration =
    try Reader.read(file)
    catch { case error: Reader.ConfigurationError => throw MessageOnlyException(error.getMessage) }
}
