package com.animatinator.scala.learn.pathfinding.traversable

import com.animatinator.scala.learn.pathfinding.Point
import org.scalatest.FunSuite

class GridMazeTest extends FunSuite {
  val emptyThreeByThree: GridMaze = GridMaze.empty(3, 3)
  val pointsToSet: List[Point] = List(Point(2, 0), Point(0, 1), Point(1, 1), Point(0, 2), Point(2, 2))
  val threeByThreeWithWalls: GridMaze = new GridMaze(
    List(
      List(false, false, true),
      List(true, true, false),
      List(true, false, true)))

  test("illegalConstruction") {
    assertThrows[IllegalArgumentException](new GridMaze(Nil))
    assertThrows[IllegalArgumentException](new GridMaze(List.fill(3)(Nil)))
  }

  test("setInGrid") {
    val after = List(
      List(false, false, false),
      List(false, false, true),
      List(false, false, false)
    )
    assert(GridMaze.setInGrid(emptyThreeByThree.grid, Point(2, 1)) == after)
  }

  test("withWalls") {
    assert(emptyThreeByThree.withWalls(pointsToSet) == threeByThreeWithWalls)
  }

  test("getAdjacent") {
    assert(threeByThreeWithWalls.getAdjacent(Point(1, 1)) == List(Point(2,1), Point(1,2), Point(1,0)))
    assert(threeByThreeWithWalls.getAdjacent(Point(1, 2)) == Nil)
  }

  test("getAdjacent_outOfRange") {
    assert(threeByThreeWithWalls.getAdjacent(Point(-2, -2)) == Nil)
    assert(threeByThreeWithWalls.getAdjacent(Point(1, -1)) == List(Point(1, 0)))
    assert(threeByThreeWithWalls.getAdjacent(Point(-1, 0)) == List(Point(0, 0)))
  }
}
