ThisBuild / scalaVersion   := "3.9.0"
ThisBuild / baumkuchenMode := BaumkuchenMode.Dev

// An aggregated project with configurations but none of the JVM settings baumkuchen builds on, as a native toolchain
// project has.
lazy val toolchain = project.disablePlugins(plugins.JvmPlugin, plugins.IvyPlugin).configs(Compile, Test)
lazy val root      = project.in(file(".")).aggregate(toolchain)
