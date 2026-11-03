package demo;

/** The entry point of the build, which uses the class of the other module. */
public class App {

  public static void main(String[] args) {
    System.out.println(new Library().name());
  }
}
