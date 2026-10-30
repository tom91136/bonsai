package demo;

/** The code the lints of the javac plugin find, which the analysis reports as findings of its own. */
public class Lints {

  /** The cast of a value to the type it already has is redundant. */
  public String cast(String value) {
    return (String) value;
  }
}
