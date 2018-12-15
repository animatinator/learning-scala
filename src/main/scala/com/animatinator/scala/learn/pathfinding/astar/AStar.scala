package com.animatinator.scala.learn.pathfinding.astar

import com.animatinator.scala.learn.pathfinding.Traversable

import scala.collection.mutable

object AStar {
  type Heuristic[T] = T => Int

  def updateMapWithNewNode[T](map : Map[T, Node[T]], newValue : Node[T]): Map[T, Node[T]] = {
    if ((map contains newValue.value) && map(newValue.value).cost < newValue.cost) {
      map
    } else {
      map + (newValue.value -> newValue)
    }
  }

  def unwindParentsToStart[T](node : Node[T], start : T): List[T] = {
    def unwindR(n : Node[T], path : List[T]): List[T] = n.value match {
      case v if v == start => v :: path
      case v => unwindR(n.parent.get, v :: path)
    }

    unwindR(node, Nil)
  }

  def findPath[T](world : Traversable[T], start : T, goal : T, h : Heuristic[T]): List[T] = {
    def findPathR(nodeQueue : mutable.PriorityQueue[Node[T]], map : Map[T, Node[T]]) : List[T] = {
      val curNode = nodeQueue.dequeue()
      if (curNode.value == goal) return unwindParentsToStart(curNode, start)
      val adjacent = world.getAdjacent(curNode.value) map {n => Node(n, curNode.distance + 1, h(n), Some(curNode))}
      adjacent foreach {nodeQueue.enqueue(_)}
      val newMap = adjacent.foldLeft(map){(m, cur) => {updateMapWithNewNode(m, cur)}}
      findPathR(nodeQueue, newMap)
    }

    val startNode = Node(start, 0, h(start), None)
    val queue = new mutable.PriorityQueue[Node[T]]()
    queue.enqueue(startNode)
    findPathR(queue, Map(start -> startNode))
  }
}
