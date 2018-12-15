package com.animatinator.scala.learn.pathfinding.astar

case class Node[T](value : T, distance : Int, heuristic : Int, parent : Option[Node[T]]) extends Ordered[Node[T]] {
  def cost: Int = distance + heuristic

  override def compare(that: Node[T]): Int = cost - that.cost

  override def toString: String =
    "Node{%s; dist: %d, heuristic: %d, parent: %s}".format(
      value, distance, heuristic, parent.getOrElse(this).value)
}
