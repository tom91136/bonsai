package fix

object FullyQualifiedReferenceInherited extends FullyQualifiedReferenceParent {
  val cpus = scala.sys.runtime.availableProcessors()
}
