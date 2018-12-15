package com.animatinator.scala.learn.pathfinding

import org.scalatest.FunSuite

class PointTest extends FunSuite {
  test("equality") {
    assert(Point(1, 5) == Point(1, 5))
  }

  test("pointArithmetic") {
    assert(Point(1, 2) + Point(3, 4) == Point(4, 6))
    assert(Point(3, 4) - Point(1, 2) == Point(2, 2))
  }

  test("pointArithmeticAlongAxes") {
    val p = Point(3, 3)
    assert(p -| 1 == Point(3, 2))
    assert(p +| 1 == Point(3, 4))
    assert(p -- 1 == Point(2, 3))
    assert(p +- 1 == Point(4, 3))
  }
}
