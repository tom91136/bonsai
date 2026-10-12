package net.kurobako.baumkuchen.analysis

import java.nio.file.Path

object Output {
  enum Scope(val id: String) {
    case Main extends Scope("main")
    case Test extends Scope("test")
  }
}

case class Output(module: String, scope: Output.Scope, path: Path)
