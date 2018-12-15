package com.animatinator.scala.learn.pathfinding.traversable

import com.animatinator.scala.learn.pathfinding.astar.AStar
import com.animatinator.scala.learn.pathfinding.{Point, Traversable}

object GridMaze {
  def empty(width : Int, height : Int): GridMaze = new GridMaze(List.fill(height)(List.fill(width)(false)))

  def setInGrid(grid : List[List[Boolean]], point : Point) : List[List[Boolean]] = {
    grid.updated(point.y, grid(point.y).updated(point.x, true))
  }

  def heuristicForGoal(goal : Point) : AStar.Heuristic[Point] = {p => (goal - p).manhattan}
}

class GridMaze(val grid : List[List[Boolean]]) extends Traversable[Point] {

  require(grid.nonEmpty && grid.head.nonEmpty)

  private def inRange(node : Point):Boolean =
    (node.x min node.y) >= 0 && node.y < grid.length && node.x < grid.head.length

  private def isFree(node : Point):Boolean = inRange(node) && !grid(node.y)(node.x)

  override def getAdjacent(node: Point): List[Point] = List(node +- 1, node -- 1, node +| 1, node -| 1) filter isFree

  def withWalls(walls : List[Point]): GridMaze = new GridMaze(walls.foldLeft(grid){ (g, p) => GridMaze.setInGrid(g, p)})


  override def toString: String = String.format("GridMaze{ %s }", grid)

  def ==(other : GridMaze): Boolean = grid == other.grid

  override def hashCode(): Int = grid.hashCode

  def canEqual(other : Any): Boolean = other.isInstanceOf[GridMaze]

  override def equals(obj: Any): Boolean = obj match {
    case that => canEqual(that) && this.hashCode == that.hashCode
    case _ => false
  }
}
