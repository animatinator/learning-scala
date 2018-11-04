object Greeter {
  def printGreeting(name : String): Unit = printf("Welcome %s!", name)
}

class HelloWorld(name : String) {
  import Greeter.printGreeting
  def helloWorld(): Unit = {println("Hello world!"); printGreeting(name)}
}

new HelloWorld("Scala").helloWorld()

def map[T](fn : T => T, list : List[T]) : List[T] = {
  list match {
    case x::more => fn(x) :: map(fn, more)
    case _ => List()
  }
}

map((x : Int) => x + 1, List(1, 2, 3, 4, 5))

def filter[T](fn : T => Boolean, list : List[T]) : List[T] = {
  list match {
    case x::more => if (fn(x)) x :: filter(fn, more) else filter(fn, more)
    case _ => List()
  }
}

filter((x : Int) => x % 2 == 0, List(1, 2, 3, 4, 5, 6))

def foldLeft[T](fn : (T, T) => T, list : List[T], acc : T) : T = {
  list match {
    case x::more => foldLeft(fn, more, fn(acc, x))
    case _ => acc
  }
}

foldLeft((x : Int, y : Int) => x + y, List(1, 2, 3, 4, 5), 0)

abstract class Tree[T]
case class Node[T](left : Tree[T], value : T, right : Tree[T]) extends Tree[T] {
  def withLeftChild(newLeft : Tree[T]): Node[T] = {
    left match {
      case Node(_, _, _) => throw new Exception("Already has a left child")
      case Leaf() => Node(newLeft, value, right)
    }
  }

  def withRightChild(newRight : Tree[T]): Node[T] = {
    right match {
      case Node(_, _, _) => throw new Exception("Already has a left child")
      case Leaf() => Node(left, value, newRight)
    }
  }
}
case class Leaf[T]() extends Tree[T]

def treeSum(tree : Tree[Int]) : Int = {
  tree match {
    case Node(left, value, right) => treeSum(left) + value + treeSum(right)
    case Leaf() => 0
  }
}

def createTreeNode[T](value : T) = Node[T](Leaf[T](), value, Leaf[T]())

treeSum(createTreeNode(5))

treeSum(
  createTreeNode(5)
    .withLeftChild(createTreeNode(4))
    .withRightChild(createTreeNode(3)))

def testTree = createTreeNode(5)
  .withLeftChild(createTreeNode(3)
    .withLeftChild(createTreeNode(2)
      .withLeftChild(createTreeNode(1)))
    .withRightChild(createTreeNode(4)))
  .withRightChild(createTreeNode(8)
    .withLeftChild(createTreeNode(7)
      .withLeftChild(createTreeNode(6)))
    .withRightChild(createTreeNode(9)
      .withRightChild(createTreeNode(10))))

treeSum(testTree)

def isWellOrdered[T <% Comparable[T]](tree : Tree[T]) : Boolean =
  wellOrderedInner(tree, None, None)

def minWithOption[T <% Ordered[T]](value : T, option : Option[T]) : T = {
  option match {
    case Some(op) => if (op < value) op else value
    case None => value
  }
}

def maxWithOption[T <% Ordered[T]](value : T, option : Option[T]) : T = {
  option match {
    case Some(op) => if (op > value) op else value
    case None => value
  }
}

def wellOrderedInner[T <% Ordered[T]](tree : Tree[T], min : Option[T], max : Option[T]) : Boolean = {
  tree match {
    case Leaf() => true
    case Node(left, value, right) =>
      def leftBound = if (min.isDefined) value > min.get else true

      def rightBound = if (max.isDefined) value < max.get else true

      def leftSubtree = wellOrderedInner(left, min, Some(minWithOption(value, max)))

      def rightSubTree = wellOrderedInner(right, Some(maxWithOption(value, min)), max)

      leftBound && rightBound && leftSubtree && rightSubTree
  }
}

isWellOrdered(createTreeNode(5).withLeftChild(createTreeNode(7)))
isWellOrdered(testTree)