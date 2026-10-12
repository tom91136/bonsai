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
package org.example.isa.ptx

import org.example.isa.ptx.asm.Printer // assert: PackageBoundary.package-boundary

object PackageBoundary {
  val imported = Printer.rendered     // assert: PackageBoundary.package-boundary
  val relative = asm.Printer.rendered // assert: PackageBoundary.package-boundary
  val allowed  = PackageBoundaryUnrelated.value
}

object PackageBoundaryUnrelated {
  val value = 1
}
