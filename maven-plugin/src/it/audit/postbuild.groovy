// The compile does not fail on the finding of the compiler plugin, and the analysis reports it with its own.
def log = new File(basedir, "build.log").text
assert log.contains("[bonsai:array-equality]") : "the compiler did not show the finding"
assert log.contains("the analysis reads") : "the analysis did not run after the compile"
assert log =~ /error\tarray-equality\t/ : "the finding of the compiler plugin is missing from the report"
assert log =~ /error\tunused-member\t/ : "the finding of the analysis is missing from the report"
assert log.indexOf("[bonsai:array-equality]") < log.indexOf("the analysis reads") : "the compile did not come first"

// The records of the compiler plugin are kept with the classes, one for each source of Scala.
def records = new File(basedir, "target/classes/META-INF/bonsai/findings").listFiles({ it.name.endsWith(".tsv") } as FileFilter)
assert records != null && records.length == 3 : "the records are missing or not one for each source"

// The second run reports the finding of the first compile, as the sources are the same.
def reported = (log =~ /error\tarray-equality\t/).count
assert reported == 2 : "the finding was reported ${reported} times, not once for each run"
