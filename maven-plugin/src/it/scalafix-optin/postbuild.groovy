// The enabled rule reports the inferred type, and the compile still passes.
def log = new File(basedir, "build.log").text
assert !log.contains("No warnings can be incurred") : "the compile failed"
assert log.contains("val names: List[String]") : "ExplicitResultTypes did not report the inferred type"
