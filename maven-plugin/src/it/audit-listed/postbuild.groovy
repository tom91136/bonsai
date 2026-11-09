// Both findings are in the report, as warnings.
def log = new File(basedir, "build.log").text
assert log =~ /warning\tarray-equality\t/ : "the finding of the compiler plugin is missing from the report"
assert log =~ /warning\tunused-member\t/ : "the finding of the analysis is missing from the report"
assert !(log =~ /error\t(array-equality|unused-member)\t/) : "a finding kept as a warning failed"
