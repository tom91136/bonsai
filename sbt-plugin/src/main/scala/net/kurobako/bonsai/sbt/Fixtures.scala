package net.kurobako.bonsai.sbt

import sbt.*
import sbt.Keys.thisProject
import sbt.internal.BuildStructure

object Fixtures {

  /** The projects that every dependant uses only from its test configurations, from (project, mapping) edges. */
  def fixtures(dependencies: Seq[(String, Option[String])]): Set[String] =
    dependencies.groupBy(_._1).collect {
      case (project, edges) if edges.forall(edge => testMapping(edge._2)) => project
    }.toSet

  def fixtures(structure: BuildStructure): Set[String] =
    fixtures(structure.allProjectRefs.flatMap { ref =>
      (ref / thisProject).get(structure.data).toList.flatMap(_.dependencies.map(dependency =>
        dependency.project.project -> dependency.configuration
      ))
    })

  private def testMapping(mapping: Option[String]): Boolean =
    mapping.exists(_.split(';').forall(_.trim.takeWhile(_ != '-') == Test.name))
}
