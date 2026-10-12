package net.kurobako.baumkuchen.analysis

case class Graph(nodes: Map[String, Node], edges: Map[String, Set[String]], roots: Set[String])
