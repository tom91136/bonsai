package demo;

/** A class the other module of the build uses, which the analysis does not report as unused because it does. */
public class Library {

  public String name() {
    return "library";
  }
}
