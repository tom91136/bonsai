package demo;

/** A class nothing refers to, which the analysis reports. It is package-private, so it is not part of the API the
  * analysis counts as used. */
class Orphan {

  String name() {
    return "orphan";
  }
}
