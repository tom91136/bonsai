package net.kurobako.bonsai.collections

/** How a method treats the elements: `Map` replaces each, `Filter` keeps or drops each, `Flat` replaces each with any
  * number, `Terminal` ends the chain with something that is not a collection, and `Other` is the rest.
  */
enum Shape {
  case Map, Filter, Flat, Terminal, Other
}
