/*
rule = NoPackagePrivate
NoPackagePrivate.tiers = ["org.tiers", "org.tiers.isa"]
 */
package org.tiers.isa.example {
  private[isa] object IsaTier
  private[tiers] object RootTier
  class TierMembers private[isa] () {
    private[tiers] def value: Int = 1
  }
  private[example] object Own // assert: NoPackagePrivate.package-private
}
