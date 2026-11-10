package net.kurobako.bonsai.effects

enum Kind(val base: String, val shown: String) {
  case Reads   extends Kind("read mutable state", "reads mutable state")
  case Writes  extends Kind("write state", "writes state")
  case Input   extends Kind("read from outside the program", "reads from outside the program")
  case Output  extends Kind("perform output", "performs output")
  case Random  extends Kind("be nondeterministic", "is nondeterministic")
  case Unknown extends Kind("run code without known effects", "runs code without known effects")
  case Throws  extends Kind("throw", "throws")
}
