package com.animatinator.scala.learn.ninetynineproblems.knightstour

import org.scalatest.FunSuite

class KnightsTourTest extends FunSuite {
  test("point_equality") {
    assert(Point(1, 2) == Point(1, 2))
    assert(Point(1, 2) != Point(3, 4))
  }

  test("point_addition") {
    assert(Point(1, 2) + Point(3, 4) == Point(4, 6))
  }

  test("point_rotateClockwise") {
    assert(Point(3, 5).rotateClockwise == Point(-5, 3))
    assert(Point(79, 52).rotateClockwise.rotateClockwise.rotateClockwise.rotateClockwise == Point(79, 52))
  }

  test("buildFunctionApplicationList_negative") {
    assert(buildFunctionApplicationList[Int](-10, {_ + 1})(3) == List(3))
  }

  test("buildFunctionApplicationList_zero") {
    assert(buildFunctionApplicationList[Int](0, {_ + 1})(3) == List(3))
  }

  test("buildFunctionApplicationList_three") {
    assert(buildFunctionApplicationList[Int](3, {_ + 1})(3) == List(3, 4, 5, 6))
  }

  test("oneJump_rightNumber") {
    assert(oneJump.length == 8)
  }

  test("oneJump_allUnique") {
    assert(oneJump.distinct == oneJump)
  }

  test("oneJumpFromPoint_arbitraryPoint") {
    assert(oneJumpFromPoint(Point(3, 4)) contains Point(1, 5))
  }

  test("isInRange_zero") {
    assert(!isInRange(0)(Point(0, 0)))
  }

  test("isInRange_oneByOne") {
    assert(isInRange(1)(Point(0, 0)))
  }

  test("isInRange_bottomRightCornerBoundary") {
    assert(isInRange(5)(Point(4, 4)))
    assert(!isInRange(5)(Point(5, 4)))
    assert(!isInRange(5)(Point(4, 5)))
  }

  test("jumps_noSpace") {
    assert(jumps(1, Point(0, 0)).isEmpty)
  }

  test("jumps_stillNoSpace") {
    assert(jumps(2, Point(0, 0)).isEmpty)
  }

  test("jumps_corner") {
    assert(jumps(3, Point(0, 0)).length == 2)
  }

  test("jumps_edge") {
    assert(jumps(10, Point(0, 5)).length == 4)
  }

  test("jumps_loadsOfSpace") {
    assert(jumps(10, Point(5, 5)).length == 8)
  }
}
