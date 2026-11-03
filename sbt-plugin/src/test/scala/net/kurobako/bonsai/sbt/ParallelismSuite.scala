package net.kurobako.bonsai.sbt

import sbt.Tags

class ParallelismSuite extends munit.FunSuite {

  test("a build that restricts nothing may use every processor") {
    assertEquals(Parallelism.of(Nil, 12), 12)
  }

  test("the default restriction of the processors of the machine allows them all") {
    assertEquals(Parallelism.of(Seq(Tags.limitAll(12), Tags.limit(Tags.ForkedTestGroup, 1)), 12), 12)
  }

  test("a build that runs one task at a time gets one thread") {
    assertEquals(Parallelism.of(Seq(Tags.limitAll(1)), 12), 1)
  }

  test("a limit on all tasks below the processors is followed, and above them is not exceeded") {
    assertEquals(Parallelism.of(Seq(Tags.limitAll(3)), 12), 3)
    assertEquals(Parallelism.of(Seq(Tags.limitAll(32)), 12), 12)
  }

  test("the tightest of several limits is followed") {
    assertEquals(Parallelism.of(Seq(Tags.limitAll(6), Tags.limit(Tags.All, 4), Tags.limitAll(9)), 12), 4)
  }

  test("a limit on tasks of other tags does not narrow the threads") {
    assertEquals(Parallelism.of(Seq(Tags.limit(Tags.Compile, 2), Tags.limit(Tags.Test, 1)), 12), 12)
  }

  test("a rule that allows no concurrency leaves one thread, as sbt always runs a task alone") {
    assertEquals(Parallelism.of(Seq(Tags.customLimit(_ => false)), 12), 1)
  }
}
