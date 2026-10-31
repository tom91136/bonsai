package net.kurobako.bonsai.java

object Descriptors {

  val Object: String = "Ljava/lang/Object;"

  def primitive(keyword: String): String = keyword match {
    case "boolean" => "Z"
    case "byte"    => "B"
    case "short"   => "S"
    case "int"     => "I"
    case "long"    => "J"
    case "char"    => "C"
    case "float"   => "F"
    case "double"  => "D"
    case "void"    => "V"
  }

  def array(element: String): String = s"[$element"

  def clazz(binary: String): String = s"L${binary.replace('.', '/')};"

  def key(owner: String, name: String, parameters: Seq[String], result: String): String =
    s"$owner.$name(${parameters.mkString})$result"
}
