package demo

object Hand {
  def read(s: String): Either[String, Int] = try Right(Integer.parseInt(s)) catch { case _: Throwable => Left(s) }
}
