package net.kurobako.bonsai.sbt

import net.kurobako.bonsai.configuration.BonsaiConfiguration as Reader
import sbt.*

/** `.bonsai.conf`, read by the reader the sbt plugin shares with the other tools, which fails the build with its
  * message.
  */
object BonsaiConfiguration {

  type Analysis      = Reader.Analysis
  type Configuration = Reader.Configuration
  type Settings      = Reader.Settings

  val Analysis: Reader.Analysis.type           = Reader.Analysis
  val Configuration: Reader.Configuration.type = Reader.Configuration
  val Settings: Reader.Settings.type           = Reader.Settings
  val DefaultBaseline: String                  = Reader.DefaultBaseline
  val DefaultPurity: String                    = Reader.DefaultPurity
  val OptionFolds: Seq[String]                 = Reader.OptionFolds

  /** The glob of the sources a build generates into `directory`, relative to the `root` sources are named from. */
  def generatedGlob(root: File, directory: File): String =
    IO.relativize(root, directory).getOrElse(directory.getAbsolutePath) + "/**"

  def read(file: File): Configuration =
    try Reader.read(file)
    catch { case error: Reader.ConfigurationError => Compat.fail(error.getMessage) }
}
