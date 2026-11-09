def log = new File(basedir, "build.log").text
assert !log.contains("scalafix-cli") : "scalafix ran on an excluded module"
