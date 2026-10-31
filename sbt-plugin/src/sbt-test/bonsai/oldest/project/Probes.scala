import sbt.*
import sbt.Keys.*

object Probes extends AutoPlugin {
  override def trigger = allRequirements

  private val organization = "net.kurobako"
  private val version      = sys.props("plugin.version")
  private val fixtures     = file(sys.props("bonsai.fixtures"))

  private case class Line(name: String, oldest: String) {
    private val Array(major, minor, patch) = oldest.split('.')
    def id: String                         = s"${major}_$minor"
    def previous: Option[String]           = Option.when(patch.toInt > 0)(s"$major.$minor.${patch.toInt - 1}")
  }

  private val lines = sys.props("bonsai.oldest").split(',').toSeq.map(_.split(':')).map(pair => Line(pair(0), pair(1)))

  private def probe(id: String, scala: String, plugin: Option[Line]): Project =
    Project(id, file(id)).settings(
      scalaVersion                         := scala,
      Compile / unmanagedSourceDirectories := Seq("collections", "effects").map(fixtures / _ / "src/main/scala"),
      libraryDependencies ++= plugin.toSeq.map(line =>
        compilerPlugin((organization % "bonsai-plugin" % version).cross(CrossVersion.constant(line.name)))
      )
    )

  private val oldest = lines.flatMap(line => Seq(probe(s"oldest_${line.id}", line.oldest, Some(line))))

  private val controls = lines.flatMap(line => line.previous.map(previous => probe(s"plain_${line.id}", previous, None)))

  private val below = lines.flatMap(line => line.previous.map(previous => probe(s"below_${line.id}", previous, Some(line))))

  override def extraProjects: Seq[Project] = oldest ++ controls ++ below

  private def compileAll(name: String, projects: Seq[Project], failing: Boolean) = Command.command(name) { state =>
    val extracted = Project.extract(state)
    val unexpected = projects.map(_.id).filter { id =>
      scala.util.Try(extracted.runTask(LocalProject(id) / Compile / compile, state)).isSuccess == failing
    }
    if (unexpected.isEmpty) state
    else {
      state.log.error(s"$name: ${unexpected.mkString(", ")}")
      state.fail
    }
  }

  override def globalSettings: Seq[Setting[?]] =
    Seq(commands ++= Seq(compileAll("oldestCompiles", oldest ++ controls, failing = false), compileAll("belowFails", below, failing = true)))
}
