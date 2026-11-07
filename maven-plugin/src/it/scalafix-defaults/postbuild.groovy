// The compile passes under -Werror, and the rules report the unused import and the inferred type.
def log = new File(basedir, "build.log").text
assert !log.contains("No warnings can be incurred") : "the unused warning failed the compile"
assert log.contains("-import scala.util.Try") : "RemoveUnused did not report the unused import"
assert log.contains("val names: List[String]") : "ExplicitResultTypes did not report the inferred type"
