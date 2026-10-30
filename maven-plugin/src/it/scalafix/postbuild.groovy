// The build fails on the scalafix finding of the second source.
def log = new File(basedir, "build.log").text
assert log.contains("[MutableExposure.global-state]") : "the scalafix finding is missing from the build log"
