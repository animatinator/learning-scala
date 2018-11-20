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

  def pack[T](list : List[T]) : List[List[T]] = {
    // Note: turns out this method already exists in the form of List.span(pred).
    def prefixMatching(list: List[T], pred: T => Boolean): (List[T], List[T]) = list match {
      case x :: tail if pred(x) => val (subresult, rem) = prefixMatching(tail, pred); (x :: subresult, rem)
      case somethingElse => (Nil, somethingElse)
    }

    if (list.isEmpty) Nil else {
      val (firstBlock, rest) = prefixMatching(list, (x : T) => x == list.head)
      firstBlock :: pack(rest)
    }
  }

  def encode[T](list : List[T]) : List[(Int, T)] = {
    pack(list) map {xs => (xs.length, xs.head)}
  }

  def encodeModified[T](list : List[T]) : List[Any] = {
    encode(list).map {pair => if (pair._1 > 1) pair else pair._2}
  }

  def decode[T](list : List[(Int, T)]) : List[T] = list flatMap {pair => List.fill(pair._1)(pair._2)}

  def encodeDirect[T](list : List[T]) : List[(Int, T)] = {
    if (list.isEmpty) Nil
    else {
      val (initial, rest) = list span (_ == list.head)
      (initial.length, list.head) :: encodeDirect(rest)
    }
  }

  def duplicate[T](list : List[T]) : List[T] = list match {
    case x :: xs => x :: x :: duplicate(xs)
    case Nil => Nil
  }

  def duplicateN[T](n : Int, list : List[T]) : List[T] = list flatMap {List.fill(n)(_)}

  def drop[T](n : Int, list : List[T]) : List[T] = {
    def dropInner(c : Int, list : List[T]) : List[T] = (c, list) match {
      case (1, _ :: xs) => dropInner(n, xs)
      case (_, x :: xs) => x :: dropInner(c - 1, xs)
      case (_, Nil) => Nil
    }

    dropInner(n, list)
  }

  def split[T](n : Int, list : List[T]) : (List[T], List[T]) = (n, list) match {
    case (_, Nil) => (Nil, Nil)
    case (0, xs) => (Nil, xs)
    case (c, x :: xs) =>
      val (first, second) = split(c - 1, xs)
      (x :: first, second)
  }
}
