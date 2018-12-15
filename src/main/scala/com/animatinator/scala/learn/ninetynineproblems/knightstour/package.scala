package com.animatinator.scala.learn.ninetynineproblems

package object knightstour {
  case class Point(x : Int, y : Int) {
    def +(other : Point) : Point = Point(x + other.x, y + other.y)
    def -(other : Point) : Point = Point(x - other.x, y - other.y)

    def flipY : Point = Point(x, -y)
    def flipX : Point = Point(-x, y)

    def rotateClockwise : Point = Point(-y, x)
  }

  def buildFunctionApplicationList[T](n : Int, f : T => T)(x : T) : List[T] = {
    if (n <= 0) List(x)
    else {
      val fx = f(x)
      x :: buildFunctionApplicationList(n - 1, f)(fx)
    }
  }

  val knightMovePattern = Point(2, 1)

  def oneJump : List[Point] =
    List(knightMovePattern, knightMovePattern.flipY) flatMap buildFunctionApplicationList(3, {v : Point => v.rotateClockwise})

  def oneJumpFromPoint(p : Point) : List[Point] = oneJump map {p + _}

  def isInRange(boardSize : Int)(p : Point) : Boolean = p.x >= 0 && p.x < boardSize && p.y >= 0 && p.y < boardSize

  def jumps(n : Int, p : Point) : List[Point] = oneJumpFromPoint(p) filter isInRange(n)
}
