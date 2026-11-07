package net.kurobako.bonsai.analysis

/** Where the object an instruction acts on came from: the receiver of the method, an object or array the method
  * allocated, an exception a handler caught, or anything else, which includes every parameter and every value read from
  * the heap.
  */
private[analysis] enum Origin {
  case This, Fresh, Other, Caught
}
