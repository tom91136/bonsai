// The project list as plan.scala and report.scala read it: one JSON object per line, or one JSON array.

val Majors = Set("8", "11", "17", "21", "25")

def text(value: ujson.Value): String = value match {
  case ujson.Str(s) => s
  case ujson.Num(n) => if (n.isWhole) n.toLong.toString else n.toString
  case other        => ujson.write(other)
}

def optional(project: ujson.Value, key: String): String = project.obj.get(key) match {
  case None | Some(ujson.Null) | Some(ujson.False) => ""
  case Some(value)                                 => text(value)
}

def derivedName(url: String): String =
  url
    .replaceFirst("/+$", "")
    .replaceFirst("\\.git$", "")
    .replaceFirst(".*[:/]([^/]+/[^/]+)$", "$1")
    .replace("/", "__")

def nameOf(project: ujson.Value): String = {
  val name = optional(project, "name")
  if (name.nonEmpty) name else derivedName(optional(project, "url"))
}

def parseProjects(source: String): Vector[ujson.Value] = {
  val trimmed = source.trim
  if (trimmed.startsWith("[")) ujson.read(trimmed).arr.toVector
  else trimmed.linesIterator.filterNot(_.isBlank).map(ujson.read(_)).toVector
}

def projectErrors(projects: Vector[ujson.Value]): Vector[String] = {
  val each = projects.zipWithIndex.flatMap { (project, i) =>
    val label = s"project ${i + 1}"
    project.objOpt.fold(Vector(s"$label: not an object")) { fields =>
      Vector(
        Option.when(!fields.get("url").exists(_.strOpt.isDefined))(s"$label: url is not a string"),
        Option.when(!fields.get("ref").exists(_.strOpt.isDefined))(s"$label: ref is not a string"),
        Option.when(!Majors.contains(optional(
          project,
          "java"
        )))(s"$label: java is not one of ${Majors.toVector.sortBy(_.toInt).mkString(", ")}"),
        Option.when(
          fields.get("sbt").exists(sbt => sbt != ujson.Null && sbt.arrOpt.isEmpty)
        )(s"$label: sbt is not an array")
      ).flatten
    }
  }
  val names = projects.filter(_.objOpt.isDefined).map(nameOf)
  val clash =
    names.groupBy(identity).collect { case (name, all) if all.size > 1 => s"name $name is used ${all.size} times" }
  each ++ clash.toVector.sorted
}
