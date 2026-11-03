def log = new File(basedir, "build.log").text

// The analysis runs once, whichever module finishes last.
def runs = (log =~ /bonsai: the analysis reads \S*manifest\.tsv/).count
assert runs == 1 : "the analysis ran " + runs + " times"
def manifests = ["", "lib", "app", "tiny"].collect { new File(basedir, it + "/target/bonsai/manifest.tsv") }.findAll { it.exists() }
assert manifests.size() == 1 : "the manifest of the build was written " + manifests.size() + " times"

// The analysis reads the fragment of every module, whatever order the modules finished in.
def manifest = manifests[0].readLines()
["lib", "app", "tiny"].each { module ->
  assert manifest.any { it.startsWith("output\t" + module + "\tmain\t") } : "the manifest does not hold the output of " + module
}

// The class the other module uses is not reported as unused, and the class nothing refers to is.
assert !(log =~ /\t(unused|test-only)\t[^\t]*\t[^\t]*\tdemo\.Library\t/) : "the analysis reported the class the other module uses as unused"
assert log.contains("demo.Orphan") : "the analysis did not report the class nothing refers to"
