/*
rule = PackageBoundary
PackageBoundary.boundaries = [
  {
    sources = "isa/ptx/other/"
    forbidden = ["org/example/isa/ptx/asm/"]
  },
  {
    sources = "does/not/match/"
    forbidden = ["org/example/isa/ptx/asm/"]
  }
]
 */
package org.example.isa.ptx.other

import org.example.isa.ptx.asm.Emitter // assert: PackageBoundary.package-boundary
import org.example.isa.ptx.asm.Printer // assert: PackageBoundary.package-boundary

object NoExempt {
  val imported = Printer.rendered // assert: PackageBoundary.package-boundary
  def make: Emitter = null        // assert: PackageBoundary.package-boundary
}
