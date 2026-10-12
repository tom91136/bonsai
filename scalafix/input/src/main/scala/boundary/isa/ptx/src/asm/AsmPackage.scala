/*
rule = PackageBoundary
PackageBoundary.boundaries = [
  {
    sources = "isa/ptx/src/"
    exempt = "/asm/"
    forbidden = ["org/example/isa/ptx/asm/"]
    message = "runtime code must not reference the asm package"
  }
]
 */
package org.example.isa.ptx.asm

class Emitter

object Printer {
  val rendered = 1
}

object AsmUses {
  val inside = Printer.rendered
}
