package com.animatinator.scala.learn.pathfinding.astar

case class Node[T](value : T, distance : Int, heuristic : Int, parent : Option[Node[T]]) extends Ordered[Node[T]] {
  def cost: Int = distance + heuristic

  def this(value : T, distance : Int, heuristic : Int) = this(value, distance, heuristic, this)

  override def compare(that: Node[T]): Int = cost - that.cost

  override def toString: String =
    String.format("Node{%s; dist: %s, heuristic: %s, parent: %s}",
      value, distance, heuristic, parent.getOrElse(this).value)
}
