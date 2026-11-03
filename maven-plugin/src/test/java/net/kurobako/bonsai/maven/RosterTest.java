package net.kurobako.bonsai.maven;

import org.eclipse.aether.DefaultRepositorySystemSession;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RosterTest {

  @Test
  void theModuleThatCompletesTheSetIsTheOneToRun() {
    Roster roster = new Roster();

    assertFalse(roster.record("a", 3));
    assertFalse(roster.record("b", 3));
    assertTrue(roster.record("c", 3));
  }

  @Test
  void theOrderTheModulesFinishInDoesNotMatter() {
    Roster roster = new Roster();

    assertFalse(roster.record("last", 2));
    assertTrue(roster.record("first", 2));
  }

  @Test
  void aLoneModuleRunsTheAnalysis() {
    assertTrue(new Roster().record("a", 1));
  }

  @Test
  void aModuleThatRecordsAgainDoesNotRunTheAnalysisAgain() {
    Roster roster = new Roster();

    assertTrue(roster.record("a", 1));
    assertFalse(roster.record("a", 1));
  }

  @Test
  void aSetThatIsNotCompleteNeverRuns() {
    Roster roster = new Roster();

    assertFalse(roster.record("a", 3));
    assertFalse(roster.record("b", 3));
  }

  @Test
  void exactlyOneOfTheThreadsRecordingTheModulesIsToldToRun() throws Exception {
    int modules = 16;
    for (int round = 0; round < 1000; round++) {
      Roster roster = new Roster();
      CountDownLatch start = new CountDownLatch(1);
      ExecutorService pool = Executors.newFixedThreadPool(modules);
      try {
        List<Future<Boolean>> results = new ArrayList<>();
        for (int i = 0; i < modules; i++) {
          String module = "m" + i;
          results.add(pool.submit(() -> {
            start.await();
            return roster.record(module, modules);
          }));
        }
        start.countDown();
        int runs = 0;
        for (Future<Boolean> result : results) {
          if (result.get()) {
            runs++;
          }
        }
        assertEquals(1, runs);
      } finally {
        pool.shutdownNow();
      }
    }
  }

  @Test
  void aSessionHasOneRosterWhichEveryModuleOfItSees() {
    DefaultRepositorySystemSession session = new DefaultRepositorySystemSession();

    assertSame(Roster.of(session), Roster.of(new DefaultRepositorySystemSession(session)));
  }

  @Test
  void anotherSessionStartsWithAnEmptyRoster() {
    Roster.of(new DefaultRepositorySystemSession()).record("a", 2);

    assertFalse(Roster.of(new DefaultRepositorySystemSession()).record("b", 2));
    assertTrue(Roster.of(new DefaultRepositorySystemSession()).record("a", 1));
  }
}
