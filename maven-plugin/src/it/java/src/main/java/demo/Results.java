package demo;

/** The code whose result is never used, which the analysis reports of its own. */
public class Results {

  /** The result of the call is dropped at every call, which the analysis reports. */
  public void run() {
    value();
    value();
  }

  /** The value the code does not use. */
  private String value() {
    return "value";
  }
}
