def log = new File(basedir, "build.log").text
assert !log.contains("extraction failed") : "the analysis could not read the sources of the module"

// The path the compiler records for a source is from the root of the build, so the analysis, which runs there, finds it.
def tasty = new File(basedir, "core/target/classes/demo/Aaa.tasty").getText("ISO-8859-1")
assert tasty.contains("core/src/main/scala/demo/Aaa.scala") : "the compiler recorded the source from the module"
