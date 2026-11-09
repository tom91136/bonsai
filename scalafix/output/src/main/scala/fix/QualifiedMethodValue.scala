package fix

import java.nio.file.Files
import java.nio.file.Path

object QualifiedMethodValue {
  val paths: List[Path] = Nil
  val sizes = paths.map((
    Files).size(_))
}
