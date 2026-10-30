/*
rule = InterpolationIdioms
 */
// format: off
package fix

object InterpolationIdiomsInput {
  val bytesField = "x"
  val field = Field("x")

  val pointless = s"static_cast<size_t>(${bytesField}.size())"// assert: InterpolationIdioms.redundant-interpolation
  val rawPointless = raw"${bytesField}.size()"// assert: InterpolationIdioms.redundant-interpolation
  val terminal = s"${bytesField}"// assert: InterpolationIdioms.redundant-interpolation
  val punctuation = s"${bytesField}!"// assert: InterpolationIdioms.redundant-interpolation
  val adjacent = s"${bytesField}${field.name}"// assert: InterpolationIdioms.redundant-interpolation

  val suffixed = s"${bytesField}_suffix"
  val selected = s"${field.name}.size()"
  val already = s"$bytesField.size()"

  final case class Field(name: String)
}
