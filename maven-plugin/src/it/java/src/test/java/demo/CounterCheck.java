package demo;

/** A check of the counter of the demo, which the model of the test code holds. */
public class CounterCheck {

  /** Checks that the counter counts. */
  public boolean check() {
    return new Counter().increment() == 1;
  }
}
