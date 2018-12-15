package com.animatinator.scala.learn.pathfinding.traversable

import org.scalatest.FunSuite

class TraverableListTest extends FunSuite {
  val sampleList = new TraversableList[Int](List(1, 2, 3, 4, 5))

  test("emptyList") {
    assert(new TraversableList[Int](Nil).getAdjacent(1) == Nil)
  }

  test("endOfList") {
    assert(sampleList.getAdjacent(5) == Nil)
  }

  test("midList") {
    assert(sampleList.getAdjacent(3) == List(4))
  }
}
