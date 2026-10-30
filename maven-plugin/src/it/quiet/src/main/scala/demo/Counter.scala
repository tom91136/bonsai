package demo

final class Counter {

  private var count: Int = 0

  def increment(): Int = {
    count = count + 1
    count
  }
}
