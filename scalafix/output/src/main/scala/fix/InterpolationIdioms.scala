// format: off
package fix

object InterpolationIdiomsInput {
  val bytesField = "x"
  val field = Field("x")

  val pointless = s"static_cast<size_t>($bytesField.size())"
  val rawPointless = raw"$bytesField.size()"
  val terminal = s"$bytesField"
  val punctuation = s"$bytesField!"
  val adjacent = s"$bytesField${field.name}"

  val suffixed = s"${bytesField}_suffix"
  val selected = s"${field.name}.size()"
  val already = s"$bytesField.size()"

  final case class Field(name: String)
}
