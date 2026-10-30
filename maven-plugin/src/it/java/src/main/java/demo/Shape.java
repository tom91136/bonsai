package demo;

/** A sealed type, which the analysis reads as a sum of the types that extend it, as it reads a sealed trait of Scala
 * code. */
public sealed interface Shape permits Circle, Square {}
