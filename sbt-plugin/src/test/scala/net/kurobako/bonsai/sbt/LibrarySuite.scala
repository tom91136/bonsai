package net.kurobako.bonsai.sbt

class LibrarySuite extends munit.FunSuite {

  test("the configuration decides whatever the project looks like") {
    assert(Library.detect(Some(true), skipped = true, declared = false, mainClasses = Seq("demo.Main")))
    assert(!Library.detect(Some(false), skipped = false, declared = true, mainClasses = Nil))
  }

  test("a project skipped from publishing is no library, whatever it declares") {
    assert(!Library.detect(None, skipped = true, declared = true, mainClasses = Nil))
    assert(!Library.detect(None, skipped = true, declared = false, mainClasses = Nil))
  }

  test("a published project that declares its licences, developers or homepage is a library with main classes too") {
    assert(Library.detect(None, skipped = false, declared = true, mainClasses = Seq("demo.Main")))
    assert(Library.detect(None, skipped = false, declared = true, mainClasses = Nil))
  }

  test("without such declarations a published project is a library when it has no main class") {
    assert(Library.detect(None, skipped = false, declared = false, mainClasses = Nil))
    assert(!Library.detect(None, skipped = false, declared = false, mainClasses = Seq("demo.Main")))
  }
}
