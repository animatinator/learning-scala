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

  test("pack_empty") {
    assert(ListProblems.pack(Nil) == Nil)
  }

  test("pack_oneElement") {
    assert(ListProblems.pack(List(1)) == List(List(1)))
  }

  test("pack_singles") {
    assert(ListProblems.pack(List(1, 2, 3)) == List(List(1), List(2), List(3)))
  }

  test("pack_groups") {
    assert(ListProblems.pack(List(1, 1, 2, 3, 3)) == List(List(1, 1), List(2), List(3, 3)))
  }

  test("encode_empty") {
    assert(ListProblems.encode(Nil) == Nil)
  }

  test("encode_singleElement") {
    assert(ListProblems.encode(List(1)) == List((1, 1)))
  }

  test("encode_severalElements") {
    assert(ListProblems.encode(List(1, 1, 1, 2, 2, 3, 3, 4)) == List((3, 1), (2, 2), (2, 3), (1, 4)))
  }

  test("encodeModified_mixture") {
    assert(ListProblems.encodeModified(List(1, 1, 2, 3, 3)) == List((2, 1), 2, (2, 3)))
  }

  test("decode_empty") {
    assert(ListProblems.decode(Nil) == Nil)
  }

  test("decode_singles") {
    assert(ListProblems.decode(List((1, 1), (1, 2), (1, 3))) == List(1, 2, 3))
  }

  test("decode_mixed") {
    assert(ListProblems.decode(List((3, 1), (2, 2), (1, 3))) == List(1, 1, 1, 2, 2, 3))
  }

  test("encodeDirect_empty") {
    assert(ListProblems.encodeDirect(Nil) == Nil)
  }

  test("encodeDirect_singleElement") {
    assert(ListProblems.encodeDirect(List(1)) == List((1, 1)))
  }

  test("encodeDirect_severalElements") {
    assert(ListProblems.encodeDirect(List(1, 1, 1, 2, 2, 3, 3, 4)) == List((3, 1), (2, 2), (2, 3), (1, 4)))
  }

  test("duplicate_empty") {
    assert(ListProblems.duplicate(Nil) == Nil)
  }

  test("duplicate_someList") {
    assert(ListProblems.duplicate(List(1, 2, 3, 3, 4)) == List(1, 1, 2, 2, 3, 3, 3, 3, 4, 4))
  }

  test("duplicateN_someList") {
    assert(ListProblems.duplicateN(4, List(2, 5)) == List(2, 2, 2, 2, 5, 5, 5, 5))
  }

  test("drop_empty") {
    assert(ListProblems.drop(3, Nil) == Nil)
  }

  test("drop_everything") {
    assert(ListProblems.drop(1, List(1, 2, 3)) == Nil)
  }

  test("drop_nBiggerThanLength") {
    assert(ListProblems.drop(100, List(1, 2, 3)) == List(1, 2, 3))
  }

  test("drop_standardCase") {
    assert(ListProblems.drop(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9)) == List(1, 2, 4, 5, 7, 8))
  }

  test("split_empty") {
    assert(ListProblems.split(3, Nil) == (Nil, Nil))
  }

  test("split_longerThanList") {
    assert(ListProblems.split(3, List(1, 2)) == (List(1, 2), Nil))
  }

  test("split_emptyFirstPart") {
    assert(ListProblems.split(0, List(1, 2, 3)) == (Nil, List(1, 2, 3)))
  }

  test("split_normalCase") {
    assert(ListProblems.split(3, List(1, 2, 3, 4, 5)) == (List(1, 2, 3), List(4, 5)))
  }
}
