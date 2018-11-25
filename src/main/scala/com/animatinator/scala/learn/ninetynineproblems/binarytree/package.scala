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
    def height : Int
    def heightOfLeftmostPoint : Int
    def bounds : List[(Int, Int)]

    def addValue[U >: T](value : U)(implicit ord: U => Ordered[U]) : Tree[U] = this match {
      case End => Node(value)
      case Node(v, l, r) => if (value < v) Node(v, l.addValue(value), r) else Node(v, l, r.addValue(value))
    }

    def layoutBinaryTree : Tree[T] = layoutBinaryTreeInternal(1, 1)._1
    def layoutBinaryTreeInternal(nextX : Int, y : Int) : (Tree[T], Int)
    def layoutBinaryTree2 : Tree[T] =
      layoutBinaryTreeInternal2(Math.pow(2, heightOfLeftmostPoint).toInt, 1, Math.pow(2, height - 1).toInt)
    def layoutBinaryTreeInternal2(xPosition : Int, height : Int, spacing : Int) : Tree[T]
    def layoutBinaryTree3 : Tree[T] = {
      if (bounds isEmpty) this
      else layoutBinaryTreeInternal3((bounds map {case (l, _) => -l} max) + 1, 1)
    }
    def layoutBinaryTreeInternal3(xPosition : Int, height : Int) : Tree [T]

    def preorder : List[T]
    def inorder : List[T]
  }

  // This would be a case class but we need PositionedNode to extend it. It is therefore an abstract class and the
  // companion object implements apply and unapply to allow us to use it as though it were a case class.
  class Node[+T](val value: T, val left: Tree[T], val right: Tree[T]) extends Tree[T] {
    override def toString: String = (left, right) match {
      case (End, End) => value.toString
      case _ => "%s(%s,%s)".format(value.toString, left.toString, right.toString)
    }
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

    override def height: Int = 1 + Math.max(left.height, right.height)

    override def heightOfLeftmostPoint : Int = left.heightOfLeftmostPoint + 1

    override def bounds : List[(Int, Int)] =(left.bounds, right.bounds) match {
      case (Nil, Nil) => List((0, 0))
      case (leftBounds, Nil) => (0, 0) :: (leftBounds map {case (l, r) => (l - 1, r - 1)})
      case (Nil, rightBounds) => (0, 0) :: (rightBounds map {case (l, r) => (l + 1, r + 1)})
      case (leftBounds, rightBounds) =>
        val minimumDistance = Tree.minimumDistanceBetweenTreesWithBounds(leftBounds, rightBounds)
        val shiftFactor = (minimumDistance + 1) / 2 // Round up

        val leftOptionals = leftBounds map {l => Some(l)}
        val rightOptionals = rightBounds map {r => Some(r)}

        (0, 0) :: leftOptionals.zipAll(rightOptionals, None, None).map{
          case (Some(l), None) => (l._1 - shiftFactor, l._2 - shiftFactor)
          case (None, Some(r)) => (r._1 + shiftFactor, r._2 + shiftFactor)
          case (Some(l), Some(r)) => (l._1 - shiftFactor, r._2 + shiftFactor)
        }
    }

    override def layoutBinaryTreeInternal(nextX: Int, y : Int): (Tree[T], Int) = {
      val (laidOutLeft, thisX) = left.layoutBinaryTreeInternal(nextX, y + 1)
      val (laidOutRight, finalLeft) = right.layoutBinaryTreeInternal(thisX + 1, y + 1)
      (PositionedNode(value, laidOutLeft, laidOutRight, thisX, y), finalLeft)
    }

    override def layoutBinaryTreeInternal2(xPosition: Int, height : Int, spacing: Int): Tree[T] = {
      PositionedNode(value,
        left.layoutBinaryTreeInternal2(xPosition - spacing, height + 1, spacing / 2),
        right.layoutBinaryTreeInternal2(xPosition + spacing, height + 1, spacing / 2),
        xPosition, height)
    }

    override def layoutBinaryTreeInternal3(xPosition: Int, height: Int): Tree[T] = {
      bounds match {
        case _ :: (l, r) :: _ =>
          PositionedNode(value,
            left.layoutBinaryTreeInternal3(xPosition + l, height + 1),
            right.layoutBinaryTreeInternal3(xPosition + r, height + 1),
            xPosition, height)
        case _ => PositionedNode(value, End, End, xPosition, height)
      }
    }

    override def preorder: List[T] = value :: left.preorder ::: right.preorder

    override def inorder : List[T] = left.inorder ::: List(value) ::: right.inorder

    override def equals(obj: scala.Any): Boolean = obj match {
      case obj : Node[T] => obj.value == value && obj.left == left && obj.right == right
      case _ => false
    }
  }

  case class PositionedNode[+T](override val value: T,
                                override val left: Tree[T],
                                override val right: Tree[T],
                                x: Int,
                                y: Int) extends Node[T](value, left, right) {
    override def toString : String =
      "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + string(left) + " " + string(right) + ")"

    // Exists so PositionedNodes' empty children are printed correctly
    private def string[U](child : Tree[U]) : String = child match {
      case End => "."
      case _ => child.toString
    }
  }

  case object End extends Tree[Nothing] {
    override def toString = ""
    override def reflect : Tree[Nothing] = End
    override def isMirrorImage[V](other: Tree[V]): Boolean = other == End
    override def isSymmetric: Boolean = true
    override def size = 0
    override def leafCount = 0
    override def leafList: List[Nothing] = Nil
    override def internalList: List[Nothing] = Nil
    override def atLevel(level : Int): List[Nothing] = Nil
    override def height : Int = 0
    override def bounds : List[(Int, Int)] = Nil
    override def heightOfLeftmostPoint : Int = 0
    override def layoutBinaryTreeInternal(nextX: Int, y : Int): (Tree[Nothing], Int) = (End, nextX)
    override def layoutBinaryTreeInternal2(xPosition: Int, height: Int, spacing: Int): Tree[Nothing] = End
    override def layoutBinaryTreeInternal3(xPosition: Int, height: Int): Tree[Nothing] = End
    override def preorder : List[Nothing] = Nil
    override def inorder : List[Nothing] = Nil
  }

  object Node {
    def apply[T](value: T): Node[T] = new Node(value, End, End)
    def apply[T](value : T, l : Tree[T], r : Tree[T]) = new Node(value, l, r)
    def unapply[U] (node: Node[U]) : Option[(U, Tree[U], Tree[U])] = Some((node.value, node.left, node.right))
  }

  object Even {
    def unapply(x : Int): Option[Int] = if (x % 2 == 0) Some(x) else None
  }

  object Odd {
    def unapply(x : Int): Option[Int] = if (x % 2 == 1) Some(x) else None
  }

  object Parser {
    implicit def toParser(string : String) : Parser = new Parser(string)
  }

  class Parser(string: String) {
    var index : Int = 0

    def isEmpty : Boolean = index >= string.length

    def expectSymbol(symbol : Char): Unit = {
      if (string(index) != symbol)
        throw new IllegalStateException("Tried to read '%s' but found '%s'".format(symbol, string(index)))
      index += 1
    }

    def readChar : Char = {
      val result : Char = string(index)
      index += 1
      result
    }

    def peek : Char = string(index)
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

    def completeBinaryTree[T](nodes : Int, value : T) : Tree[T] = {
      def leftChildIndex(index : Int) = index * 2
      def rightChildIndex(index : Int) = index * 2 + 1

      def treeFromList(rootIndex : Int) : Tree[T] =
        if (rootIndex > nodes) End
        else Node(value, treeFromList(leftChildIndex(rootIndex)), treeFromList(rightChildIndex(rootIndex)))

      treeFromList(1)
    }

    // Given two sets of tree bounds, find the minimum distance between the roots of those trees.
    // Bounds are lists [(left, right)] specifying the distance of the left and right extremes at each level of the tree
    // from the root node.
    // * Bounds for a single node are List((0, 0)).
    // * Bounds for T (T (. .) T (. .)) are List((0, 0), (-1, 1))
    // Separated out for testing.
    def minimumDistanceBetweenTreesWithBounds(leftBounds : List[(Int, Int)], rightBounds : List[(Int, Int)]) : Int = {
      val minSpacingBetweenTrees : Int = leftBounds zip rightBounds map {case (p1, p2) => p1._2 - p2._1 + 1} max;
      if (minSpacingBetweenTrees > 2) minSpacingBetweenTrees else 2
    }

    def fromString(parser : Parser) : Tree[Char] = {
      if (parser.isEmpty) return End
      if (parser.peek == ',' || parser.peek == ')') return End
      val value : Char = parser.readChar
      if (!parser.isEmpty && parser.peek == '(') {
        parser.expectSymbol('(')
        val left : Tree[Char] = fromString(parser)
        parser.expectSymbol(',')
        val right : Tree[Char] = fromString(parser)
        parser.expectSymbol(')')
        Node(value, left, right)
      } else Node(value)
    }
  }
}
