import java.io.File

object ManifestShape {
  private val Directories = """(^|[:\t])(\$\{[A-Z_]+\}|/)[^:]*/""".r

  private def last(path: String): String = path.substring(path.lastIndexOf('/') + 1)

  def of(lines: Seq[String], root: File): Seq[String] = {
    val base = root.getCanonicalPath
    val shaped = lines.flatMap { line =>
      line.split("\t", -1).toList match {
        case "output" :: project :: scope :: classes :: Nil => Seq(s"output\t$project\t$scope\t${last(classes)}")
        case "jdk" :: home :: Nil                           =>
          assert(new File(home, "bin/java").exists, s"no JDK at $home")
          Seq("jdk")
        case "classpath" :: project :: entry :: Nil =>
          if (entry.startsWith(base)) Nil else Seq(s"classpath\t$project\t${last(entry)}")
        case "source" :: project :: scope :: source :: Nil =>
          Seq(s"source\t$project\t$scope\t${source.stripPrefix(base + "/")}")
        case "root" :: directory :: Nil =>
          assert(new File(directory).getCanonicalPath == base, s"root $directory is not $base")
          Seq("root")
        case _ => Seq(Directories.replaceAllIn(line, "$1"))
      }
    }
    val (outputs, rest) = shaped.partition(_.startsWith("output\t"))
    outputs.sorted ++ rest
  }
}
