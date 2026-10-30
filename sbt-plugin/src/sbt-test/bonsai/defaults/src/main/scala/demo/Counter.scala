package demo

class Counter {
  private var count = 0

  def next(): Int = {
    count += 1
    count
  }
}
