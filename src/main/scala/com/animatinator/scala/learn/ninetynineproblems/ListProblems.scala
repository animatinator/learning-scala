package com.animatinator.scala.learn.ninetynineproblems

import scala.util.Random

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
    // Catch the weird edge case where the input 'n' is negative.
    case (c, xs) if c < 0 => (Nil, xs)
    case (c, x :: xs) =>
      val (first, second) = split(c - 1, xs)
      (x :: first, second)
  }

  def slice[T](i : Int, j : Int, list : List[T]) : List[T] = {
    val (front, _) = split(j, list)
    val (_, result) = split(i, front)
    result
  }

  def rotate[T](distance : Int, list : List[T]): List[T] = {
    var adjustedDistance = if (list.isEmpty) 0 else distance % list.size
    if (adjustedDistance < 0) adjustedDistance = adjustedDistance + list.size
    val (front, back) = split(adjustedDistance, list)
    back ::: front
  }

  def removeAt[T](k : Int, list : List[T]) : (List[T], T) = {
    val valueAt = list(k)
    ((list take k) ::: (list drop (k + 1)), valueAt)
  }

  def insertAt[T](value : T, k : Int, list : List[T]) : List[T] = {
    val (start, end) = list splitAt k
    start ::: (value :: end)
  }

  def range(i : Int, j : Int) : List[Int] = (i, j) match {
    case (x, y) if y < x => Nil
    case _ => i :: range(i + 1, j)
  }

  def randomSelect[T](n : Int, list : List[T]) : List[T] = {
    if (list.isEmpty || n <= 0) return Nil
    val (remainder, value) = removeAt((new Random).nextInt(list.length), list)
    value :: randomSelect(n - 1, remainder)
  }

  def lotto(num : Int, max : Int) : List[Int] = randomSelect(num, range(1, max))

  def randomPermute[T](list : List[T]) : List[T] = randomSelect(list.length, list)

  def combinations[T](num : Int, list : List[T]) : List[List[T]] = {
    def flatMapSublists[A, B](ls : List[A])(f : List[A] => List[B]) : List[B] = ls match {
      case Nil => Nil
      case subList@_ :: tail => f(subList) ::: flatMapSublists(tail)(f)
    }

    num match {
      case 0 => List(Nil)
      case n => flatMapSublists(list){ls => combinations(n - 1, ls.tail) map {ls.head :: _}}
    }
  }

  // TODO: P27, group3 and group

  def lsort[T](list : List[List[T]]) : List[List[T]] = list.sortWith((l1, l2) => l1.length < l2.length)

  def lsortFreq[T](list : List[List[T]]) : List[List[T]] = {
    val frequenciesMap = list.groupBy[Int](ls => ls.length) map {pair => (pair._1, pair._2.length)}
    list.sortWith((l1, l2) => frequenciesMap(l1.length) < frequenciesMap(l2.length))
  }
}
