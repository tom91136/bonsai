package demo;

/** A record that extends the sealed type, which the analysis reads as a case class. */
public record Square(double side) implements Shape {}
