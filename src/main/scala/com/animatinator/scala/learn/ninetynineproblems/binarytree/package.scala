package com.animatinator.scala.learn.ninetynineproblems

package binarytree {

  sealed abstract class Tree[+T] {
    def reflect : Tree[T]
    def isMirrorImage[V](other : Tree[V]) : Boolean
    def isSymmetric : Boolean
    def size : Int
    def leafCount : Int
    def leafList : List[T]
    def internalList : List[T]
    def atLevel(level : Int) : List[T]

    def addValue[U >: T](value : U)(implicit ord: U => Ordered[U]) : Tree[U] = this match {
      case End => Node(value)
      case Node(v, l, r) => if (value < v) Node(v, l.addValue(value), r) else Node(v, l, r.addValue(value))
    }
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    override def reflect : Node[T] = Node(value, right.reflect, left.reflect)
    override def isMirrorImage[V](other : Tree[V]): Boolean = other match {
      case Node(_, l, r) => l.isMirrorImage(right) && r.isMirrorImage(left)
      case _ => false
    }
    override def isSymmetric : Boolean = left.isMirrorImage(right)
    override def size: Int = 1 + left.size + right.size

    override def leafCount : Int = this match {
      case Node(_, End, End) => 1
      case Node(_, l, r) => l.leafCount + r.leafCount
    }

    override def leafList: List[T] = this match {
      case Node(_, End, End) => List(value)
      case Node(_, l, r) => l.leafList ::: r.leafList
    }

    override def internalList: List[T] = this match {
      case Node(_, End, End) => Nil
      case Node(v, l, r) => v :: l.internalList ::: r.internalList
    }

    override def atLevel(level: Int): List[T] = {
      if (level == 1) List(value)
      else left.atLevel(level - 1) ::: right.atLevel(level - 1)
    }
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
    override def reflect : Tree[Nothing] = End
    override def isMirrorImage[V](other: Tree[V]): Boolean = other == End
    override def isSymmetric: Boolean = true
    override def size = 0
    override def leafCount = 0
    override def leafList: List[Nothing] = Nil
    override def internalList: List[Nothing] = Nil
    override def atLevel(level : Int): List[Nothing] = Nil
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Even {
    def unapply(x : Int): Option[Int] = if (x % 2 == 0) Some(x) else None
  }

  object Odd {
    def unapply(x : Int): Option[Int] = if (x % 2 == 1) Some(x) else None
  }

  object Tree {
    def cBalanced[T](nodes : Int, value : T) : List[Tree[T]] =
      if (nodes == 0) List(End)
      else if (nodes == 1) List(Node(value))
      else {
        nodes match {
          case Odd(x) =>
            val subTrees: List[Tree[T]] = cBalanced((x - 1) / 2, value)
            for (l <- subTrees; r <- subTrees) yield Node(value, l, r)
          case Even(x) =>
            val largerSubtrees : List[Tree[T]] = cBalanced(x / 2, value)
            val smallerSubtrees : List[Tree[T]] = cBalanced(x / 2 - 1, value)
            smallerSubtrees.flatMap(l => largerSubtrees.flatMap(r => List(Node(value, l, r), Node(value, r, l))))
        }
      }

    def fromList[T](list : List[T])(implicit ord: T => Ordered[T]) : Tree[T] =
      list.foldLeft[Tree[T]](End){_.addValue(_)}

    def symmetricBalancedTrees[T](nodes : Int, value : T): List[Tree[T]] =
      cBalanced(nodes, value) filter {_.isSymmetric}

    def hbalTrees[T](height : Int, value : T) : List[Tree[T]] =
      if (height == 0) List(End)
      else if (height == 1) List(Node(value))
      else {
        val bigSubtrees : List[Tree[T]] = hbalTrees(height - 1, value)
        val smallSubtrees : List[Tree[T]] = hbalTrees(height - 2, value)
        bigSubtrees.flatMap(l => bigSubtrees.flatMap(r => List(Node(value, l, r)))) :::
        bigSubtrees.flatMap(l => smallSubtrees.flatMap(r => List(Node(value, l, r), Node(value, r, l))))
      }

    def maxHbalNodes(height : Int) : Int = Math.pow(2, height).toInt - 1

    def minHbalNodes(height : Int) : Int = {
      if (height == 0) 0
      else if (height == 1) 1
      else {
        minHbalNodes(height - 1) + minHbalNodes(height - 2) + 1
      }
    }

    def minHbalHeight(nodes : Int): Int = Stream from 1 dropWhile {maxHbalNodes(_) < nodes} head

    def maxHbalHeight(nodes : Int): Int = Stream from 1 takeWhile {minHbalNodes(_) <= nodes} last

    def hbalTreesWithNodes[T](nodes : Int, value : T) : List[Tree[T]] =
      (minHbalHeight(nodes) to maxHbalHeight(nodes)) flatMap {hbalTrees(_, value)} filter {_.size == nodes} toList
  }
}
