package demo;

/** A class whose code repeats the code of the counter, which the analysis reports as a copy of it. */
public class Report {

  private int count;

  public int increment() {
    count = count + 1;
    return count;
  }

  /** The code of this helper is the code of the helper of the counter, which the analysis reports as a copy of it. */
  static String describe(String name) {
    StringBuilder result = new StringBuilder();
    result.append(name);
    result.append(':');
    return result.toString();
  }
}
