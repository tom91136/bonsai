// The compiler collects the model of the Java code of the main sources and of the test sources.
def main = new File(basedir, "target/bonsai/model/main.tsv")
def test = new File(basedir, "target/bonsai/model/test.tsv")
assert main.exists() : "the model of the main code is missing"
assert test.exists() : "the model of the test code is missing"
def nodes = main.readLines()
assert nodes.any { it.startsWith("node\tdemo.Counter\t") } : "the model does not hold the counter"
def testNodes = test.readLines()
assert testNodes.any { it.startsWith("node\tdemo.CounterCheck\t") } : "the model of the test code does not hold the check"
assert !testNodes.any { it.startsWith("node\tdemo.Counter\t") } : "the model of the test code holds the main code"

// The analysis reads the model, so the manifest names it and the analysis reports the class nothing refers to.
def manifest = new File(basedir, "target/bonsai/manifest.tsv").readLines()
assert manifest.any { it.startsWith("model\tjava\tmain\t") } : "the manifest does not name the model"
def log = new File(basedir, "build.log").text
assert log.contains("demo.Unused") : "the analysis did not read the model of the Java code"

// The rules read the model of the Java code, so the flag every caller passes as a literal is reported.
assert log.contains("flag-argument") : "the analysis did not report the flag of the Java code"

// The rules read the model of the Java code, so the code that recurs in it is reported as a copy of itself.
assert log.contains("duplicate-code") : "the analysis did not report the code that recurs in the Java code"

// The lints the javac plugin finds in the Java code are reported as findings of the analysis.
assert log.contains("redundant-cast") : "the analysis did not report the lint of the Java code"

// The rules that need every call site of the build read the Java code alike with the Scala code, so the result of a
// call the code never uses is reported.
assert log.contains("ignored-result") : "the analysis did not report the result the Java code drops"

// The records and the sealed types of the Java code are the products and the sums the rules read alike with those of
// the Scala code.
def products = main.readLines().findAll { it.startsWith("product\t") }
assert products.any { it.contains("demo.Point") } : "the model does not hold the record"
def sums = main.readLines().findAll { it.startsWith("sum\t") }
assert sums.any { it.contains("demo.Shape") } : "the model does not hold the sealed type"
