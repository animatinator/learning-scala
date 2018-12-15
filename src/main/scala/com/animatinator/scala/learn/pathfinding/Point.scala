package com.animatinator.scala.learn.pathfinding

case class Point(x : Int, y : Int) {
  def +(other : Point): Point = Point(x + other.x, y + other.y)
  def -(other : Point): Point = Point(x - other.x, y - other.y)
  def +|(dist : Int): Point = Point(x, y + dist)
  def +-(dist : Int): Point = Point(x + dist, y)
  def -|(dist : Int): Point = this +| -dist
  def --(dist : Int): Point = this +- -dist
}
