import sbt.*

// Stands in for Scala Native's plugin, which bonsai recognises by name.
object ScalaNativePlugin extends AutoPlugin {
  override def trigger = noTrigger
}
