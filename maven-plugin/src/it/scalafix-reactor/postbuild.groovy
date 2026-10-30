def log = new File(basedir, "build.log").text
assert log.contains("[MutableExposure.global-state]") : "the scalafix finding of the module is missing from the build log"
assert !log.contains("Stale SemanticDB") : "the rules read a stale SemanticDB"
assert !log.contains("SemanticDB not found") : "the rules found no SemanticDB"
