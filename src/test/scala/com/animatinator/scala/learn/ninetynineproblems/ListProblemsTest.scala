package com.animatinator.scala.learn.ninetynineproblems

import org.scalatest.FunSuite

class ListProblemsTest extends FunSuite {
  test("last_empty") {
    assert(ListProblems.last(Nil).isEmpty)
  }

  test("last_singleElement") {
    assert(ListProblems.last(List(3)).contains(3))
  }

  test("last_severalElements") {
    assert(ListProblems.last(List(1, 2, 3, 4, 5)).contains(5))
  }

  test("penultimate_empty") {
    assert(ListProblems.penultimate(Nil).isEmpty)
  }

  test("penultimate_singleElement") {
    assert(ListProblems.penultimate(List(3)).isEmpty)
  }

  test("penultimate_severalElements") {
    assert(ListProblems.penultimate(List(1, 2, 3, 4, 5)).contains(4))
  }

  test("nth_empty") {
    assert(ListProblems.nth(Nil, 0).isEmpty)
  }

  test("nth_overshoot") {
    assert(ListProblems.nth(List(1, 2, 3), 3).isEmpty)
  }

  test("nth_inRange") {
    assert(ListProblems.nth(List(1, 2, 3), 1).contains(2))
  }

  test("length_empty") {
    assert(ListProblems.length(Nil) == 0)
  }

  test("length_nonEmpty") {
    assert(ListProblems.length(List(1, 2, 3)) == 3)
  }

  test("reverse_empty") {
    assert(ListProblems.reverse(Nil) == Nil)
  }

  test("reverse_multiple") {
    assert(ListProblems.reverse(List(1, 2, 3, 4, 5)) == List(5, 4, 3, 2, 1))
  }

  test("isPalindrome_empty") {
    assert(ListProblems.isPalindrome(Nil))
  }

  test("isPalindrome_oneElement") {
    assert(ListProblems.isPalindrome(List(1)))
  }

  test("isPalindrome_nonPalindrome") {
    assert(!ListProblems.isPalindrome(List(1, 2, 3, 4, 5)))
  }

  test("isPalindrome_palindrome") {
    assert(ListProblems.isPalindrome(List(1, 2, 3, 2, 1)))
  }

  test("flatten_empty") {
    assert(ListProblems.flatten(Nil) == Nil)
  }

  test("flatten_simple") {
    assert(ListProblems.flatten(List(1, 2, 3)) == List(1, 2, 3))
  }

  test("flatten_mixed") {
    assert(ListProblems.flatten(List(1, 2, List(3, 4, List(5)))) == List(1, 2, 3, 4, 5))
  }

  test("compress_empty") {
    assert(ListProblems.compress(Nil) == Nil)
  }

  test("compress_noDuplicates") {
    assert(ListProblems.compress(List(1, 2, 3)) == List(1, 2, 3))
  }

  test("compress_separatedDuplicates") {
    assert(ListProblems.compress(List(1, 2, 1, 3, 1)) == List(1, 2, 1, 3, 1))
  }

  test("compress_allDuplicates") {
    assert(ListProblems.compress(List(1, 1, 1)) == List(1))
  }

  test("compress_groupedDuplicates") {
    assert(ListProblems.compress(List(1, 1, 2, 3, 2, 2, 4, 4, 5)) == List(1, 2, 3, 2, 4, 5))
  }
}
