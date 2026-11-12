package net.kurobako.bonsai.analysis

import java.nio.file.Path

object Output {
  type Part = (String, Scope)

  enum Scope(val id: String) {
    case Main extends Scope("main")
    case Test extends Scope("test")
  }
}

case class Output(module: String, scope: Output.Scope, path: Path) {
  def part: Output.Part = module -> scope
}
