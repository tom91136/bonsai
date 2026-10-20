package net.kurobako.baumkuchen.plugin

/** How a check reports by default: an error fails in every mode, and information never fails. */
private enum Severity {
  case Error, Warning, Info
}
