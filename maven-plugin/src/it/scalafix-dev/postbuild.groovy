def log = new File(basedir, "build.log").text
assert log.contains("[MutableExposure.global-state]") : "the scalafix finding is missing from the build log"
assert log.contains("bonsai: the scalafix rules report findings") : "the findings did not warn"
