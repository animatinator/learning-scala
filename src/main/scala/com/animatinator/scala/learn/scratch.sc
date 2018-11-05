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

val testTree = createTreeNode(5)
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


abstract class Expression
case class Sum(left : Expression, right : Expression) extends Expression
case class Var(n : String) extends Expression
case class Constant(i : Int) extends Expression

type Environment = String => Int

def eval(expression : Expression, environment : Environment) : Int = expression match {
  case Constant(i) => i
  case Var(n) => environment(n)
  case Sum(left, right) => eval(left, environment) + eval(right, environment)
}

val testExp = Sum(Sum(Var("x"), Var("x")), Sum(Constant(7), Var("y")))
val testEnv : Environment = {case "x" => 5; case "y" => 7}

eval(testExp, testEnv)

def derive(expression: Expression, variable: String) : Expression = expression match {
  case Sum(left, right) => Sum(derive(left, variable), derive(right, variable))
  case Var(n) if n == variable => Constant(1)
  case _ => Constant(0)
}

derive(testExp, "x")

def simplify(expression: Expression) : Expression = expression match {
  case Sum(left, right) =>
    (simplify(left), simplify(right)) match {
      case (Constant(x), Constant(y)) => Constant(x + y)
      case (simpLeft, simpRight) => Sum(simpLeft, simpRight)
    }
  case other => other
}

simplify(derive(testExp, "x"))
simplify(derive(testExp, "y"))


abstract class BinaryTree[T <% Ordered[T]] {
  def insert(item : T) : BinaryTree[T] = this match {
    case BinaryNode(value, left, right) =>
      if (item < value) BinaryNode(value, left.insert(item), right)
      else BinaryNode(value, left, right.insert(item))
    case BinaryLeaf() => BinaryNode(item, BinaryLeaf(), BinaryLeaf())
  }

  def contains(item : T) : Boolean = this match {
    case BinaryNode(value, left, right) =>
      (item == value) || (left contains item) || (right contains item)
    case BinaryLeaf() => false
  }

  def map[U <% Ordered[U]](f : T => U) : BinaryTree[U] = this match {
    case BinaryNode(value, left, right) => BinaryNode(f(value), left map f, right map f)
    case BinaryLeaf() => BinaryLeaf[U]()
  }
}
case class BinaryNode[T <% Ordered[T]](value : T, left : BinaryTree[T], right : BinaryTree[T]) extends BinaryTree[T]
case class BinaryLeaf[T <% Ordered[T]]() extends BinaryTree[T]

val testBinaryTree = BinaryLeaf[Int]()
  .insert(5)
  .insert(3)
  .insert(6)
  .insert(4)
  .insert(2)
  .insert(11)
  .insert(9)

(testBinaryTree contains 2) && (testBinaryTree contains 6) && (testBinaryTree contains 11)

testBinaryTree map (1 +)

def minVal[T <% Ordered[T]](tree: BinaryTree[T]) : T = tree match {
  case BinaryNode(value, left, _) =>
    if (left.isInstanceOf[BinaryNode[T]]) minVal(left) else value
  case _ => throw new Exception("A leaf has no minimum value!");
}

minVal(testBinaryTree)

def removeMinimumValue[T <% Ordered[T]](tree : BinaryTree[T]) : BinaryTree[T] = tree match {
  case BinaryNode(value, left, right) =>
    if (left.isInstanceOf[BinaryNode[T]]) BinaryNode(value, removeMinimumValue(left), right)
    else right
}

val testBinaryTreeAfterRemovals = removeMinimumValue(removeMinimumValue(testBinaryTree))

val minOfTestBinaryTreeAfterRemovals = minVal(testBinaryTreeAfterRemovals)

def removeRoot[T <% Ordered[T]](tree : BinaryTree[T]) : BinaryTree[T] = tree match {
  case BinaryLeaf() => BinaryLeaf()
  case BinaryNode(_, baseLeft, baseRight) =>
    (baseLeft, baseRight) match {
      case (BinaryLeaf(), right) => right
      case (left, BinaryLeaf()) => left
      case (left, right) =>
        val newRoot = minVal(right)
        BinaryNode(newRoot, left, removeMinimumValue(right))
    }
}

def size[T <% Ordered[T]](tree : BinaryTree[T]) : Int = tree match {
  case BinaryLeaf() => 0
  case BinaryNode(_, left, right) => 1 + size(left) + size(right)
}

val testBinaryTreeWithRootRemoved = removeRoot(testBinaryTree)
size(testBinaryTree)
size(testBinaryTreeWithRootRemoved)

removeRoot[Int](BinaryNode(3, BinaryNode(2, BinaryLeaf(), BinaryLeaf()), BinaryLeaf()))
removeRoot[Int](BinaryNode(3, BinaryLeaf(), BinaryNode(2, BinaryLeaf(), BinaryLeaf())))

def removeFromBinaryTree[T <% Ordered[T]](tree : BinaryTree[T], toRemove : T) : BinaryTree[T] = tree match {
  case BinaryLeaf() => BinaryLeaf()
  case BinaryNode(value, left, right) => if (value == toRemove) {
    removeRoot(BinaryNode(value, left, right))
  } else {
    BinaryNode(value, removeFromBinaryTree(left, toRemove), removeFromBinaryTree(right, toRemove))
  }
}

size(removeFromBinaryTree(testBinaryTree, 4))