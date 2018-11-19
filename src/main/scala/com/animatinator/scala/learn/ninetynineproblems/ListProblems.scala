package com.animatinator.scala.learn.ninetynineproblems

object ListProblems {
  def last[T](list : List[T]) : Option[T] = list match {
    case head :: Nil => Some(head)
    case _ :: tail => last(tail)
    case Nil => None
  }

  def penultimate[T](list : List[T]) : Option[T] = list match {
    case head :: _ :: Nil => Some(head)
    case _ :: tail => penultimate(tail)
    case Nil => None
  }

  def nth[T](list : List[T], n : Int) : Option[T] = (list, n) match {
    case (head :: _, 0) => Some(head)
    case (_ :: tail, c) => nth(tail, c - 1)
    case _ => None
  }

  def length[T](list : List[T]) : Int = list.foldLeft(0) {(c, _) => c + 1}

  def reverse[T](list : List[T]) : List[T] = list.foldLeft[List[T]](Nil) {(list, current) => current :: list}

  def isPalindrome[T](list : List[T]) : Boolean = reverse(list) == list

  def flatten(list : List[Any]) : List[Any] = list flatMap {
    case list : List[_] => flatten(list)
    case item => List(item)
  }

  def compress[T](list : List[T]) : List[T] = list.foldRight[List[T]](Nil) {
    (current, list) =>
      if (list.isEmpty || list.head != current) current :: list
      else list
  }
}
