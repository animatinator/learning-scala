package com.animatinator.scala.learn.pathfinding.astar

import org.scalatest.FunSuite

class AStarTest extends FunSuite {
  test("nodeOrdering") {
    assert(Node(1, 1, 1, None) < Node(1, 2, 2, None))
  }

  test("unwindParentsToStart_alreadyThere") {
    val a = Node(1, 1, 1, None)
    assert(AStar.unwindParentsToStart(a, 1) == List(1))
  }

  test("unwindParentsToStart_someSteps") {
    val a = Node(1, 1, 1, None)
    val b = Node(2, 1, 1, Some(a))
    val c = Node(3, 1, 1, Some(b))
    val d = Node(4, 1, 1, Some(c))
    assert(AStar.unwindParentsToStart(d, 1) == List(1, 2, 3, 4))
  }

  test("updateMapWithNode_replaceHigherDistance") {
    val a = Node(1, 3, 4, None)
    val b = Node(1, 2, 4, None)
    val newMap = AStar.updateMapWithNewNode(Map(1 -> a), b)
    assert(newMap(1) == b)
  }

  test("updateMapWithNode_doNotReplaceLowerDistance") {
    val a = Node(1, 3, 4, None)
    val b = Node(1, 2, 4, None)
    val newMap = AStar.updateMapWithNewNode(Map(1 -> b), a)
    assert(newMap(1) == b)
  }
}
