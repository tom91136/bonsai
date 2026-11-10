import java.io.File
import java.lang.foreign.Arena
import java.lang.foreign.FunctionDescriptor
import java.lang.foreign.Linker
import java.lang.foreign.ValueLayout
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import sbt.*
import sbt.Keys.*
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.sys.process.Process
import scala.sys.process.ProcessLogger

/** Installs the snapshot where other builds resolve it: `installLocal` publishes it to a staging directory next to the
  * Ivy repository, exchanges each module into the repository in a single rename, and, where Maven is installed,
  * installs the tools into the Maven repository.
  *
  * The Ivy repository is `local` under the Ivy home of sbt, `-Dsbt.ivy.home`, and the Maven repository the one Maven
  * names, `-Dmaven.repo.local`, so that a run can be pointed away from the home directory.
  */
object InstallLocal {

  private val Stage    = "installLocalStage"
  private val Exchange = "installLocalExchange"
  private val Discard  = "installLocalDiscard"

  private val StagePrefix = ".stage-"

  /** The commands of `installLocal`, which publishes by running `steps`. A `++` step drops the settings that stage the
    * publishing, so each is followed by staging again.
    */
  def commands(steps: Seq[String]): Seq[Command] = Seq(
    Command.command("installLocal") { state =>
      val extracted = Project.extract(state)
      val home      = ivyHome(extracted.get(LocalRootProject / ivyPaths))
      Files.createDirectories(home.toPath)
      val stage   = Files.createTempDirectory(home.toPath, StagePrefix)
      val staging = s"$Stage $stage"
      val publish = steps.flatMap(step => if (step.startsWith("++")) Seq(step, staging) else Seq(step))
      state.copy(
        onFailure = Some(Exec(s"$Discard $stage", None)),
        remainingCommands = (staging +: publish :+ s"$Exchange $stage").map(Exec(_, None)).toList ++
          state.remainingCommands
      )
    },
    Command.single(Stage) { (state, stage) =>
      val extracted = Project.extract(state)
      val paths     = IvyPaths(extracted.get(ThisBuild / baseDirectory).toString, Some(stage))
      extracted.appendWithSession(extracted.structure.allProjectRefs.map(ref => (ref / ivyPaths) := paths), state)
    },
    Command.single(Exchange) { (state, stage) =>
      val log       = state.log
      val extracted = Project.extract(state)
      val staged    = new File(stage, "local")
      val target    = new File(new File(stage).getParentFile, "local")
      val modules   = versionDirectories(staged)
      modules.foreach { version =>
        val relative    = staged.toPath.relativize(version)
        val destination = target.toPath.resolve(relative)
        Files.createDirectories(destination.getParent)
        if (Files.isDirectory(destination)) exchange(version, destination)
        else Files.move(version, destination, StandardCopyOption.ATOMIC_MOVE)
        log.info(s"published $relative")
      }
      IO.delete(new File(stage))
      val cleared = state.copy(onFailure = None)
      maven(extracted, modules.map(staged.toPath.relativize), target, log) match {
        case Right(_)      => cleared
        case Left(failure) => log.error(failure); cleared.fail
      }
    },
    Command.single(Discard) { (state, stage) =>
      IO.delete(new File(stage))
      state.fail
    }
  )

  private def ivyHome(paths: IvyPaths): File =
    paths.ivyHome.map(new File(_)).getOrElse(new File(sys.props("user.home"), ".ivy2"))

  /** A version directory is the one that holds the `ivys` directory, whatever its depth: sbt 1 plugins publish under
    * `<org>/<module>/scala_<scala>/sbt_<sbt>/<version>`, everything else under `<org>/<module>/<version>`.
    */
  private def versionDirectories(staged: File): Seq[Path] =
    if (!staged.isDirectory) Nil
    else {
      val walk = Files.walk(staged.toPath)
      try walk.iterator().asScala.filter(path => path.getFileName.toString == "ivys" && Files.isDirectory(path))
          .map(_.getParent).toSeq.sorted
      finally walk.close()
    }

  private val RenameExchange = 2
  private val RenameSwap     = 2
  private val AtFdcwd        = -100

  // glibc and musl have renameat2, macOS has renamex_np; the Java file API has no exchange
  private lazy val renameExchange: (Arena, Path, Path) => Int = {
    val linker = Linker.nativeLinker()
    val lookup = linker.defaultLookup()
    (lookup.find("renameat2").toScala, lookup.find("renamex_np").toScala) match {
      case (Some(renameat2), _) =>
        val handle = linker.downcallHandle(
          renameat2,
          FunctionDescriptor.of(
            ValueLayout.JAVA_INT,
            ValueLayout.JAVA_INT,
            ValueLayout.ADDRESS,
            ValueLayout.JAVA_INT,
            ValueLayout.ADDRESS,
            ValueLayout.JAVA_INT
          )
        )
        (arena, from, to) =>
          handle.invoke(
            AtFdcwd,
            arena.allocateFrom(from.toString),
            AtFdcwd,
            arena.allocateFrom(to.toString),
            RenameExchange
          ).asInstanceOf[Int]
      case (_, Some(renamex)) =>
        val handle = linker.downcallHandle(
          renamex,
          FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.JAVA_INT)
        )
        (arena, from, to) =>
          handle.invoke(
            arena.allocateFrom(from.toString),
            arena.allocateFrom(to.toString),
            RenameSwap
          ).asInstanceOf[Int]
      case _ => throw new UnsupportedOperationException("renameat2 or renamex_np")
    }
  }

  /** Swaps two directories in a single rename, so that a reader of either path sees the old content or the new. */
  private def exchange(from: Path, to: Path): Unit = {
    val arena = Arena.ofConfined()
    try {
      val result = renameExchange(arena, from, to)
      if (result != 0) throw new java.io.IOException(s"could not exchange $from with $to")
    } finally arena.close()
  }

  /** Maven reads its own repository layout, so the tools the Maven plugin runs are installed with the poms their build
    * published.
    */
  private def maven(extracted: Extracted, versions: Seq[Path], target: File, log: Logger): Either[String, Unit] = {
    val command = sys.env.getOrElse("MAVEN_COMMAND", "mvn")
    if (!onPath(command)) Right(())
    else {
      val organization = extracted.get(ThisBuild / Keys.organization)
      val version      = extracted.get(ThisBuild / Keys.version)
      val base         = extracted.get(ThisBuild / baseDirectory)
      val repository   = sys.props.get("maven.repo.local").map(path => s"-Dmaven.repo.local=$path").toSeq
      val plugin       = "org.apache.maven.plugins:maven-install-plugin:3.1.4"
      val artifacts    = versions.collect {
        case path
            if path.getNameCount == 3 && path.getName(0).toString == organization &&
              path.getName(2).toString == version && path.getName(1).toString.matches("""bonsai-.*_3.*""") =>
          path.getName(1).toString
      }

      def run(arguments: Seq[String], directory: File): Either[String, Unit] = {
        val output = Seq.newBuilder[String]
        val code   = Process(command +: arguments, directory).!(ProcessLogger(output += _, output += _))
        if (code == 0) Right(())
        else Left(s"$command ${arguments.mkString(" ")} failed:\n${output.result().mkString("\n")}")
      }

      // the installs below run offline, so the plugin itself has to be fetched first
      val fetched = run(Seq("-B", "-q") ++ repository ++ Seq(s"$plugin:help"), base)
      artifacts.foldLeft(fetched) { (previous, artifact) =>
        previous.flatMap { _ =>
          val source = new File(target, s"$organization/$artifact/$version")
          val result = run(
            Seq("-q", "-o") ++ repository ++ Seq(
              s"$plugin:install-file",
              s"-Dfile=${source / "jars" / s"$artifact.jar"}",
              s"-DpomFile=${source / "poms" / s"$artifact.pom"}"
            ),
            base
          )
          result.foreach(_ => log.info(s"installed $artifact $version"))
          result
        }
      }.flatMap(_ =>
        run(
          Seq("-B", "-q", "-DskipTests", "-Dinvoker.skip=true") ++ repository :+ "install",
          base / "maven-plugin"
        )
      )
    }
  }

  private def onPath(command: String): Boolean =
    if (command.contains(File.separator)) new File(command).canExecute
    else sys.env.getOrElse("PATH", "").split(File.pathSeparator).exists(directory =>
      new File(directory, command).canExecute
    )
}
