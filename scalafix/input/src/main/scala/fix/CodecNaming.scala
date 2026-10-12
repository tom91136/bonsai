/*
rule = CodecNaming
 */
// format: off
package fix {
  import sttp.tapir.Schema
  import sttp.tapir.Schema.annotations.encodedName
  import upickle.default.ReadWriter
  import upickle.implicits.key

  object CodecNaming {
    case class Tag(
        @key("userId") userId: String,// assert: CodecNaming.redundant-key
        @key("user_name") userName: String,// assert: CodecNaming.schema-name
        @key("is_locked") @encodedName("is_locked") isLocked: Boolean,
        @key("user_email") @encodedName("email") email: String,// assert: CodecNaming.schema-name
        title: String
    ) derives ReadWriter, Schema

    case class Wire(
        @key("user_name") userName: String,
        @key("id") id: Long// assert: CodecNaming.redundant-key
    ) derives ReadWriter
  }
}

package upickle {
  package implicits {
    class key(name: String) extends scala.annotation.StaticAnnotation
  }
  object default {
    trait ReadWriter[A]
    object ReadWriter { def derived[A]: ReadWriter[A] = new ReadWriter[A] {} }
  }
}

package sttp.tapir {
  trait Schema[A]
  object Schema {
    def derived[A]: Schema[A] = new Schema[A] {}
    object annotations {
      class encodedName(name: String) extends scala.annotation.StaticAnnotation
    }
  }
}
