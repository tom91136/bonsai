// The build fails on the finding the compiler plugin reports.
def log = new File(basedir, "build.log").text
assert log.contains("[baumkuchen:write-only-field]") : "the finding is missing from the build log"
