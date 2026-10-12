/*
rule = CaseClassEquality
 */
package fix {
  object CaseClassEqualityLibraries {
    case class Json(value: ujson.Value) // assert: CaseClassEquality.unstable-equality
    case class Document(
        root: ujson.Obj, // assert: CaseClassEquality.unstable-equality
        items: ujson.Arr // assert: CaseClassEquality.unstable-equality
    )
    case class Text(value: String)
  }
}

package ujson {
  sealed trait Value
  final class Obj extends Value
  final class Arr extends Value
}
