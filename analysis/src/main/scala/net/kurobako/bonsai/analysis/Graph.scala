package net.kurobako.bonsai.analysis

case class Graph(nodes: Map[String, Node], edges: Map[String, Set[String]], roots: Set[String])
