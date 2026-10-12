import sbt.*

// Stands in for Scala Native's plugin, which baumkuchen recognises by name.
object ScalaNativePlugin extends AutoPlugin {
  override def trigger = noTrigger
}
