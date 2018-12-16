package com.animatinator.scala.learn.pathfinding.astar

import com.animatinator.scala.learn.pathfinding.Point
import com.animatinator.scala.learn.pathfinding.traversable.{GridMaze, TraversableList}
import org.scalatest.FunSuite

class AStarTest extends FunSuite {
  test("nodeOrdering") {
    // The priority queue puts higher-valued items first, so this ordering is inverted.
    assert(Node(1, 1, 1, None) > Node(1, 2, 2, None))
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

  test("findPath_list") {
    val list = new TraversableList[Char](List('a', 'b', 'c', 'd'))
    val path = AStar.findPath(list, 'a', 'd', {_ : Char => 1})
    assert(path == List('a', 'b', 'c', 'd'))
  }

  test("findPath_emptyGrid") {
    val world = GridMaze.empty(5, 5)
    val path = AStar.findPath(world, Point(1, 1), Point(4, 4), GridMaze.heuristicForGoal(Point(4, 4)))
    // Note: This is a bit brittle.
    assert(path == List(Point(1,1), Point(2,1), Point(3,1), Point(3,2), Point(4,2), Point(4,3), Point(4,4)))
  }

  test("findPath_blockedGrid") {
    val world = GridMaze.empty(5, 5)
    val blockedWorld = world.withWalls(List(Point(2, 1), Point(2, 2), Point(2, 3), Point(2, 4)))
    val path = AStar.findPath(blockedWorld, Point(0, 2), Point(4, 2), GridMaze.heuristicForGoal(Point(4, 2)))
    // Note: This is a bit brittle.
    assert(path == List(
      Point(0,2), Point(1,2), Point(1,1), Point(1,0), Point(2,0), Point(3,0), Point(4,0), Point(4,1), Point(4,2)))
  }
}
