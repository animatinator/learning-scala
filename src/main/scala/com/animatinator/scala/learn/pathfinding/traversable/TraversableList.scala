package com.animatinator.scala.learn.pathfinding.traversable

import com.animatinator.scala.learn.pathfinding.Traversable

class TraversableList[T](list : List[T]) extends Traversable[T] {
  override def getAdjacent(node: T): List[T] = {
    if (!(list contains node)) Nil
    else {
      val index = list.indexOf(node)
      list.slice(index - 1, index) ::: list.slice(index + 1, index + 2)
    }
  }
}
