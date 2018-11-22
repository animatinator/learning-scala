package com.animatinator.scala.learn.ninetynineproblems

package binarytree {

  sealed abstract class Tree[+T] {
    def reflect : Tree[T]
    def isMirrorImage[V](other : Tree[V]) : Boolean
    def isSymmetric : Boolean
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    override def reflect : Node[T] = Node(value, right.reflect, left.reflect)
    override def isMirrorImage[V](other : Tree[V]): Boolean = other match {
      case Node(_, l, r) => l.isMirrorImage(right) && r.isMirrorImage(left)
      case _ => false
    }
    override def isSymmetric : Boolean = left.isMirrorImage(right)
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
    override def reflect : Tree[Nothing] = End
    override def isMirrorImage[V](other: Tree[V]): Boolean = other == End
    override def isSymmetric: Boolean = true
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
  }
}
