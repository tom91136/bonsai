package net.kurobako.bonsai.effects

enum Kind(val shown: String) {
  case Reads   extends Kind("reads mutable state")
  case Writes  extends Kind("writes state")
  case Input   extends Kind("reads from outside the program")
  case Output  extends Kind("performs output")
  case Random  extends Kind("is nondeterministic")
  case Unknown extends Kind("runs code without known effects")
  case Throws  extends Kind("throws")
}
