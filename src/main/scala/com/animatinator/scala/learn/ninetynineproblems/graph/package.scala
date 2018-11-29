package com.animatinator.scala.learn.ninetynineproblems

package object graph {

  abstract class GraphBase[T, U] {
    case class Edge(n1: Node, n2: Node, value: U) {
      def toTuple: (T, T, U) = (n1.value, n2.value, value)
    }
    case class Node(value: T) {
      var adj: List[Edge] = Nil
      // neighbors are all nodes adjacent to this node.
      def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
    }

    var nodes: Map[T, Node] = Map()
    var edges: List[Edge] = Nil

    // If the edge E connects N to another node, returns the other node,
    // otherwise returns None.
    def edgeTarget(e: Edge, n: Node): Option[Node]

    override def equals(o: Any): Boolean = o match {
      case g: GraphBase[_,_] => ((nodes.keys.toList diff g.nodes.keys.toList) == Nil) &&
        ((edges.map(_.toTuple) diff g.edges.map(_.toTuple)) == Nil)
      case _ => false
    }
    def addNode(value: T): Node = {
      val n = Node(value)
      nodes = Map(value -> n) ++ nodes
      n
    }

    def toTermForm : (List[T], List[(T, T, U)]) =
      (nodes.keys.toList, edges map {edge => (edge.n1.value, edge.n2.value, edge.value)})

    def toAdjacentForm : List[(T, List[(T, U)])] = {
      nodes.keys map {key => (key, nodes(key).adj map {x => (x.n2.value, x.value)})} toList
    }
  }

  class Graph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any): Boolean = o match {
      case g: Graph[_,_] => super.equals(g)
      case _ => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] =
      if (e.n1 == n) Some(e.n2)
      else if (e.n2 == n) Some(e.n1)
      else None

    def addEdge(n1: T, n2: T, value: U): Unit = {
      val e = Edge(nodes(n1), nodes(n2), value)
      edges = e :: edges
      nodes(n1).adj = e :: nodes(n1).adj
      nodes(n2).adj = e :: nodes(n2).adj
    }
  }

  class Digraph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any): Boolean = o match {
      case g: Digraph[_,_] => super.equals(g)
      case _ => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] =
      if (e.n1 == n) Some(e.n2)
      else None

    def addArc(source: T, dest: T, value: U): Unit = {
      val e = Edge(nodes(source), nodes(dest), value)
      edges = e :: edges
      nodes(source).adj = e :: nodes(source).adj
    }
  }

  abstract class GraphObjBase {
    type GraphClass[T, U]
    def addLabel[T](edges: List[(T, T)]): List[(T, T, Unit)] =
      edges.map(v => (v._1, v._2, ()))
    def term[T](nodes: List[T], edges: List[(T,T)]): GraphClass[T, Unit] =
      termLabel(nodes, addLabel(edges))
    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): GraphClass[T, U]
    def addAdjacentLabel[T](nodes: List[(T, List[T])]): List[(T, List[(T, Unit)])] =
      nodes.map(a => (a._1, a._2.map((_, ()))))
    def adjacent[T](nodes: List[(T, List[T])]): GraphClass[T, Unit] =
      adjacentLabel(addAdjacentLabel(nodes))
    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): GraphClass[T, U]
  }

  object Graph extends GraphObjBase {
    type GraphClass[T, U] = Graph[T, U]

    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): Graph[T, U] = {
      val g = new Graph[T, U]
      nodes.map(g.addNode)
      edges.foreach(v => g.addEdge(v._1, v._2, v._3))
      g
    }
    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): Graph[T, U] = {
      val g = new Graph[T, U]
      for ((v, _) <- nodes) g.addNode(v)
      for ((n1, a) <- nodes; (n2, l) <- a) {
        if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
          g.addEdge(n1, n2, l)
      }
      g
    }
  }

  object Digraph extends GraphObjBase {
    type GraphClass[T, U] = Digraph[T, U]

    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): Digraph[T, U] = {
      val g = new Digraph[T, U]
      nodes.map(g.addNode)
      edges.foreach(v => g.addArc(v._1, v._2, v._3))
      g
    }
    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): Digraph[T, U] = {
      val g = new Digraph[T, U]
      for ((n, _) <- nodes) g.addNode(n)
      for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
      g
    }
  }

}
