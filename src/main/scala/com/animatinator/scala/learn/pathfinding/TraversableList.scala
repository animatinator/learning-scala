package com.animatinator.scala.learn.pathfinding

class TraversableList[T](list : List[T]) extends Traversable[T] {
  override def getAdjacent(node: T): List[T] = {
    def nextAfter(elem : T, list : List[T]) : Option[T] = list match {
      case Nil => None
      case _ :: Nil => None
      case x :: y :: _ if x == elem => Some(y)
      case _ :: tail => nextAfter(elem, tail)
    }

    nextAfter(node, list).toList
  }
}
