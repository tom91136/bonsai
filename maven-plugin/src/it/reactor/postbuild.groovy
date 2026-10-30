// The analysis reads the whole build, so the class the other module of the build uses is not reported as unused.
def log = new File(basedir, "build.log").text
assert !(log =~ /\t(unused|test-only)\t[^\t]*\t[^\t]*\tdemo\.Library\t/) : "the analysis reported the class the other module uses as unused"

// Every module of the build writes its fragment of the manifest, and the analysis reads all of them.
def fragments = new File(basedir, "target/bonsai/manifests")
assert fragments.exists() : "the fragments of the modules are missing"
assert new File(fragments, "lib.tsv").exists() : "the fragment of the library is missing"
assert new File(fragments, "app.tsv").exists() : "the fragment of the application is missing"

// The modules are compiled in parallel, and each compile names its own file, so every model holds its own code.
def libModel = new File(basedir, "lib/target/bonsai/model/main.tsv")
def appModel = new File(basedir, "app/target/bonsai/model/main.tsv")
assert libModel.exists() && libModel.text.contains("demo.Library") : "the model of the library is missing or wrong"
assert appModel.exists() && appModel.text.contains("demo.App") : "the model of the application is missing or wrong"
