package fix

object FullyQualifiedReferenceShadowed {
  object Other {
    val runtime: Int = 2
  }

  trait Base {
    val runtime: Int = 0
  }

  object SameFileParent extends Base {
    val cpus = scala.sys.runtime.availableProcessors()
  }

  object Enclosing {
    val runtime: Int = 1

    class Nested {
      val cpus = scala.sys.runtime.availableProcessors()
    }
  }

  object Explicit {
    import FullyQualifiedReferenceShadowed.Other.runtime

    val cpus = scala.sys.runtime.availableProcessors() + runtime
  }

  def local: Int = {
    val runtime = 1
    scala.sys.runtime.availableProcessors() + runtime
  }

  def parameter(runtime: Int): Int = scala.sys.runtime.availableProcessors() + runtime
}
