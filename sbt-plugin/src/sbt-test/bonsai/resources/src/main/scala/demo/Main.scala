package demo

import java.util.ServiceLoader

trait Greeter {
  def greeting: String
}

class English extends Greeter {
  def greeting: String = "hello"
}

class Loud extends Greeter {
  def greeting: String = "HELLO"
}

@main def run(): Unit = ServiceLoader.load(classOf[Greeter]).forEach(greeter => println(greeter.greeting))
