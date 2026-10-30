// The build succeeds, and the tools report nothing.
def log = new File(basedir, "build.log").text
assert !log.contains("[baumkuchen:") : "the compiler plugin reported a finding"
assert log.contains("the compiler reads") : "the compiler did not read the options"
assert log.contains("the analysis reads") : "the analysis did not read the manifest"
assert log.contains("scalafix.cli.Cli") : "the scalafix rules did not run"
