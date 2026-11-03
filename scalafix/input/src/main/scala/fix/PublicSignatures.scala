/*
rule = PublicSignatures
 */
package fix

object PublicSignatures {
  def store(key: String, value: Any): Unit                 = ()     // assert: PublicSignatures.any-type
  def bounds: (Int, Int)                                   = (0, 1) // assert: PublicSignatures.tuple-type
  def resize(width: Int, height: Int, depth: Int): Unit    = ()     // assert: PublicSignatures.primitive-run
  def label(text: String, width: Int, height: Int): String = text
  def matrix(rows: Int, columns: Int, scale: Double): Unit = ()
  def assemble(source: String, tools: (assembler: String, disassembler: Option[String])): Unit = ()
  def multiplyAdd(left: Long, right: Long, addend: Long): Long = left // assert: PublicSignatures.primitive-run
  def encode(value: Long, signed: Boolean = false, octal: Boolean = false, padded: Boolean = false): String = ""
  def storeRef(value: AnyRef): Unit                                                         = ()     // assert: PublicSignatures.any-type
  def storeObject(value: java.lang.Object): Unit                                            = ()     // assert: PublicSignatures.any-type
  def indent(first: String, second: String, third: String): String                          = first  // assert: PublicSignatures.primitive-run
  private def internal(value: Any): (Int, Int)                                              = (0, 0)
  override def toString: String = "PublicSignatures"

  def plane(x: Int, y: Int, z: Int): Unit = ()

  def area(x: Int, y: Int, z: Int, w: Int): Int = x

  private object Internal {
    def resize(width: Int, height: Int, depth: Int): Unit = ()
  }

  def local(): Unit = {
    def resize(width: Int, height: Int, depth: Int): Unit = ()
    resize(width = 1, height = 2, depth = 3)
  }

  extension (value: Int) {
    def clamp(low: Int, high: Int, step: Int): Int = value // assert: PublicSignatures.primitive-run
  }
}

def topLevelPublic(value: Any): Unit = () // assert: PublicSignatures.any-type
