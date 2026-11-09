// A module is a library when it is deployed and declares licences, developers or a homepage, in its own pom or the pom
// it inherits from, and the configuration decides either way.
def fragment = { String module ->
  def file = new File(basedir, "target/bonsai/manifests/" + module + ".tsv")
  assert file.exists() : "the fragment of " + module + " is missing"
  file.readLines()
}

assert !fragment("bare").contains("library\tbare") : "a module that declares nothing is a library"
assert fragment("homepaged").contains("library\thomepaged") : "a module with a url of its own is no library"
assert fragment("inherits").contains("library\tinherits") : "a module whose parent declares licences is no library"
assert !fragment("skipped").contains("library\tskipped") : "a module skipped from deploying is a library"
assert !fragment("optout").contains("library\toptout") : "the configuration did not decide against the library"
