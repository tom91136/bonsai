// The build fails on the module that does not compile, and the analysis runs over the modules that compile.
def log = new File(basedir, "build.log").text
assert log.contains("on project broken") : "the build does not fail on the module that does not compile"
assert log =~ /bonsai: the analysis reads \S*manifest\.tsv/ : "the analysis does not run over the modules that compile"

// The fragments of the modules that compile are written, and the one of the excluded module is not.
def fragments = new File(basedir, "target/bonsai/manifests")
assert new File(fragments, "lib.tsv").exists() : "the fragment of the library is missing"
assert new File(fragments, "app.tsv").exists() : "the fragment of the application is missing"
assert !new File(fragments, "broken.tsv").exists() : "the fragment of the excluded module is written"
