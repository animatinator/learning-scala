package com.animatinator.scala.learn.pathfinding

trait Traversable[T] {
  def getAdjacent(node : T) : List[T]
}
