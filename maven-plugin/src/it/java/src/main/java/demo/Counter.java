package demo;

public class Counter {

  private int count;

  public int increment() {
    count = count + 1;
    return count;
  }

  /** The flag is a literal at every call, which the analysis reports. It is package-private, because the callers of a
   * public method in a library are not all in this build. */
  String format(String name, boolean exact) {
    return exact ? name : name.toLowerCase();
  }

  String use(String name) {
    return format(name, true) + format(name, false);
  }

  /** The code of this helper is the code of the helper of the report, which the analysis reports as a copy of it. */
  static String describe(String name) {
    StringBuilder result = new StringBuilder();
    result.append(name);
    result.append(':');
    return result.toString();
  }
}
