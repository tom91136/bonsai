// The rules left out do not run, and the compiler is not asked for unused warnings.
def log = new File(basedir, "build.log").text
assert log.contains("scalafix.cli.Cli") : "the scalafix rules did not run"
assert !log.contains("unused import") : "the compiler was asked for unused warnings"
