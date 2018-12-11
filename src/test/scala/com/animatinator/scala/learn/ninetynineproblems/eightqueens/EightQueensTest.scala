package com.animatinator.scala.learn.ninetynineproblems.eightqueens

import org.scalatest.FunSuite

class EightQueensTest extends FunSuite {
  test("allDecapitations_empty") {
    assert(allDecapitations(Nil) == Nil)
  }

  test("allDecapitations_oneElement") {
    assert(allDecapitations(List(1)) == List((1, Nil)))
  }

  test("allDecapitations_simpleList") {
    assert(allDecapitations(List(1, 2, 3)) == List((1, List(2, 3)), (2, List(1, 3)), (3, List(1, 2))))
  }

  test("generatePermuations_emptyList") {
    assert(generatePermutations(Nil) == List(Nil))
  }

  test("generatePermutations_oneElement") {
    assert(generatePermutations(List(1)) == List(List(1)))
  }

  test("generatePermutations_simpleList") {
    assert(generatePermutations(List(1, 2, 3))
      == List(List(1, 2, 3), List(1, 3, 2), List(2, 1, 3), List(2, 3, 1), List(3, 1, 2), List(3, 2, 1)))
  }

  test("splitAroundIndex_empty") {
    assertThrows[IndexOutOfBoundsException](splitAroundIndex(Nil, 0))
  }

  test("splitAroundIndex_singleElement") {
    assert(splitAroundIndex(List(1), 0) == (Nil, 1, Nil))
  }

  test("splitAroundIndex_simpleList") {
    assert(splitAroundIndex(List(1, 2, 3), 1) == (List(1), 2, List(3)))
  }

  test("splitAroundIndex_start") {
    assert(splitAroundIndex(List(1, 2, 3), 0) == (Nil, 1, List(2, 3)))
  }

  test("splitAroundIndex_end") {
    assert(splitAroundIndex(List(1, 2, 3), 2) == (List(1, 2), 3, Nil))
  }

  test("splitAroundIndex_outOfRange") {
    assertThrows[IndexOutOfBoundsException](splitAroundIndex(List(1, 2, 3), 3))
  }

  test("queenAttacksFollowingQueens_noAttack") {
    assert(!queenAttacksFollowingQueens(3, List(3, 3, 3)))
  }

  test("queenAttacksFollowingQueens_aboveDiagonal") {
    assert(queenAttacksFollowingQueens(3, List(8, 9, 0)))
  }

  test("queenAttacksFollowingQueens_belowDiagonal") {
    assert(queenAttacksFollowingQueens(3, List(-1, -5, 6)))
  }

  test("numberOfNQueensSolutions") {
    assert(nQueensSolutions.length == 92)
  }
}
