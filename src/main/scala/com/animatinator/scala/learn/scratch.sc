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

def isWellOrdered[T](tree : Tree[T])(implicit comp: T => Ordered[T]) : Boolean =
  wellOrderedInner(tree, None, None)

def minWithOption[T](value : T, option : Option[T])(implicit comp: T => Ordered[T]) : T = {
  option match {
    case Some(op) => if (op < value) op else value
    case None => value
  }
}

def maxWithOption[T](value : T, option : Option[T])(implicit comp: T => Ordered[T]) : T = {
  option match {
    case Some(op) => if (op > value) op else value
    case None => value
  }
}

def wellOrderedInner[T](tree : Tree[T], min : Option[T], max : Option[T])(implicit comp: T => Ordered[T]) : Boolean = {
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


sealed abstract class Expression
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


abstract class BinaryTree[T](implicit comp : T => Ordered[T]) {
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

  def min(): T = this match {
    case BinaryNode(value, left, _) =>
      if (left.isInstanceOf[BinaryNode[T]]) left.min() else value
    case _ => throw new Exception("A leaf has no minimum value!");
  }

  def size() : Int = this match {
    case BinaryLeaf() => 0
    case BinaryNode(_, left, right) => 1 + left.size() + right.size()
  }

  def decapitate(): BinaryTree[T] = {
    def removeMinimumValue(tree: BinaryTree[T]): BinaryTree[T] = tree match {
      case BinaryNode(value, left, right) =>
        if (left.isInstanceOf[BinaryNode[T]]) BinaryNode(value, removeMinimumValue(left), right)
        else right
    }

    this match {
      case BinaryLeaf() => BinaryLeaf()
      case BinaryNode(_, baseLeft, baseRight) =>
        (baseLeft, baseRight) match {
          case (BinaryLeaf(), right) => right
          case (left, BinaryLeaf()) => left
          case (left, right) =>
            val newRoot = right.min()
            BinaryNode(newRoot, left, removeMinimumValue(right))
        }
    }
  }

  def remove(toRemove : T) : BinaryTree[T] = this match {
    case BinaryLeaf() => BinaryLeaf()
    case BinaryNode(value, left, right) => if (value == toRemove) {
      BinaryNode(value, left, right).decapitate()
    } else {
      BinaryNode(value, left.remove(toRemove), right.remove(toRemove))
    }
  }

  def map[U](f : T => U)(implicit comp: U => Ordered[U]) : BinaryTree[U] = this match {
    case BinaryNode(value, left, right) => BinaryNode(f(value), left map f, right map f)
    case BinaryLeaf() => BinaryLeaf[U]()
  }

  def filter(p : T => Boolean) : BinaryTree[T] = this match {
    case BinaryLeaf() => BinaryLeaf()
    case BinaryNode(value, left, right) if p(value) => BinaryNode(value, left.filter(p), right.filter(p))
    case other => other.decapitate().filter(p)
  }
}
case class BinaryNode[T](value : T, left : BinaryTree[T], right : BinaryTree[T])(implicit comp: T => Ordered[T]) extends BinaryTree[T]
case class BinaryLeaf[T]()(implicit comp: T => Ordered[T]) extends BinaryTree[T]

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

val testBinaryTreeWithoutRoot = testBinaryTree.remove(5)
val testBinaryTreeWithSomeRemovals = testBinaryTree.remove(4).remove(9).remove(3)

testBinaryTreeWithoutRoot.size() == testBinaryTree.size() - 1
testBinaryTreeWithSomeRemovals.size() == testBinaryTree.size() - 3

val filteredTestBinaryTree = testBinaryTree.filter(value => (value % 2) == 0)


class InfiniteSequence[T](head : T, next : T => T) {
  def tail : InfiniteSequence[T] = new InfiniteSequence[T](next(head), next)

  override def toString = "" + head + "..."
}

val naturals = new InfiniteSequence[Int](1, n => n + 1)

naturals.tail.tail


class Point(val x : Double, val y : Double) {
  override def toString = "("+x+", "+y+")"
}

trait OperatorPoint[T <: OperatorPoint[T]] extends Point {
  def combine(second : Point, f : (Double, Double) => Double): T

  def +(other : Point): T = combine(other, (x, y) => x + y)

  def -(other : Point) : T = combine(other, (x, y) => x - y)

  def *(mult : Double) : T
}

trait MagnitudePoint extends Point {
  def magnitude = Math.sqrt((x * x) + (y * y))
}

class FancyPoint(x : Double, y : Double) extends Point(x, y) with OperatorPoint[FancyPoint] with MagnitudePoint {
  def combine(second : Point, f : (Double, Double) => Double) =
    new FancyPoint(f(x, second.x), f(y, second.y))

  def *(mult : Double) : FancyPoint = new FancyPoint(x * mult, y * mult)
}

new FancyPoint(1, 2) + new FancyPoint(3, 4)

val negativePoint = new FancyPoint(1, 2) - new FancyPoint(3, 4)

(negativePoint * 5).magnitude == negativePoint.magnitude * 5


abstract class AbsIterator {
  type T
  def hasNext : Boolean
  def next : T
}

class StringIterator(string : String) extends AbsIterator {
  type T = Char
  private var i = 0

  override def hasNext = i < string.length

  override def next = {
    val ch = string charAt i
    i += 1
    ch
  }
}

trait RichIterator extends AbsIterator {
  def foreach(f : T => Unit): Unit = while(hasNext) f(next)
}

class RichStringIter(string : String) extends StringIterator(string) with RichIterator

new RichStringIter("Hello") foreach println


val numbers = Seq(1, 2, 3, 4, 5)
numbers map (_ * 2)

numbers.foldLeft(0)(_ + 2 * _)

numbers.foldLeft(List[Int]())(_ :+ _)

(0 /: numbers)(_+_)


def isPiIsh(num : Double) = num match {
  case 3.14 => true
  case _ => false
}

isPiIsh(3.14)
isPiIsh(3.5)


val basicLinkRegex = "http://www\\.([a-zA-Z0-9]+)\\.com".r
val stringContainingBasicLink = "Search for stuff at http://www.altavista.com!"

basicLinkRegex.findAllIn(stringContainingBasicLink).subgroups foreach println


class SillyTime(val baseSeconds : Long) {
  def hours = baseSeconds / 3600
  def minutes = (baseSeconds % 3600) / 60
  def seconds = baseSeconds % 60
}

object SillyTime {
  def apply(seconds : Long) = new SillyTime(seconds)

  def unapply(time : SillyTime) : Option[(Long, Long, Long)] =
    Some((time.hours, time.seconds, time.minutes))
}

val sillyTime = SillyTime(3661)

sillyTime match {
  case SillyTime(hours, minutes, seconds) =>
    println(""+hours+":"+minutes+":"+seconds)
  case _ => println("Bad match :(")
}

object LongTime {
  def unapply(time : SillyTime) = time match {
    case SillyTime(hours, _, _) => hours > 0
    case _ => false
  }
}

sillyTime match {
  case LongTime() => println("sillyTime is a long time")
  case _ => println("Matching didn't work properly")
}

val groupList = List(List(1, 2, 3, 4, 5), List(2, 3, 4, 5, 6), List(2), List(1, 2))
groupList.groupBy[Int](ls => ls.length) map {pair => (pair._1, pair._2.length)}


def printStream[T](stream : Stream[T])(num : Int): Unit =
  stream take num foreach {x => print(x); print(", ")}

val squaresStream = Stream.from(1) map (x => x*x)
printStream(squaresStream)(20)
squaresStream.zip(squaresStream.tail) take 20

val fib : Stream[Int] = 1 #:: 1 #:: fib.zip(fib.tail).map {n => n._1 + n._2}

printStream(fib)(20)

val fib3 : Stream[Int] = 1 #:: 1 #:: 2 #:: fib3.zip(fib3.tail).map {n => n._1 + n._2}.zip(fib3.tail.tail).map { n => n._1 + n._2}

printStream(fib3)(20)

// Fun extension methods class for lists. Adds a toStream method which already existed.
object ExtendedList {
  class ExtendedList[T](list : List[T]) {
    def toAStream: Stream[T] = list match {
      case x :: xs => x #:: xs.toStream
      case Nil => Stream.empty[T]
    }
  }

  implicit def extendThatList[T](list : List[T]): ExtendedList[T] = new ExtendedList(list)
}

object ExtendedStream {
  class ExtendedStream[T](stream : Stream[T]) {
    def zip3(second : Stream[T], third : Stream[T]) : Stream[(T, T, T)] =
      stream.zip(second).zip(third) map {case ((a, b), c) => (a, b, c)}
  }

  implicit def extendStream[T](stream : Stream[T]) : ExtendedStream[T] = new ExtendedStream[T](stream)
}

import ExtendedList.extendThatList

val stream : Stream[Int] = List(1, 1, 2).toAStream

import ExtendedStream._

stream.zip3(stream, stream)
