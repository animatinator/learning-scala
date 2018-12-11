package com.animatinator.scala.learn.ninetynineproblems

package object eightqueens {

  def generatePermutations[T](list : List[T]) : List[List[T]] = {
    if (list.isEmpty) List(Nil)
    else allDecapitations(list) flatMap {case (h, tl) => generatePermutations(tl) map {h :: _}}
  }

  def splitAroundIndex[T](list : List[T], index : Int) : (List[T], T, List[T]) =
    (list.slice(0, index), list(index), list.slice(index + 1, list.length))

  def allDecapitations[T](list : List[T]) : List[(T, List[T])] = {
    list.indices.toList map {splitAroundIndex(list, _)} map {case (l, v, r) => (v, l ::: r)}
  }

  def baseNQueensPositions : List[List[Int]] = generatePermutations(List(1, 2, 3, 4, 5, 6, 7, 8))

  def queenAttacksFollowingQueens(queen : Int, following : List[Int]) : Boolean = {
    def queenAttacksR(qTop : Int, qBottom : Int, following : List[Int]) : Boolean = following match {
      case Nil => false
      case rival :: _ if rival == qTop || rival == qBottom => true
      case _ :: tail =>  queenAttacksR(qTop - 1, qBottom + 1, tail)
    }

    queenAttacksR(queen - 1, queen + 1, following)
  }

  def isValidNQueensSolution(solution : List[Int]) : Boolean = {
    solution.indices forall {
      index => {
        val (left, queen, right) = splitAroundIndex(solution, index)
        !(queenAttacksFollowingQueens(queen, left.reverse) || queenAttacksFollowingQueens(queen, right))
      }
    }
  }

  def nQueensSolutions : List[List[Int]] = baseNQueensPositions filter isValidNQueensSolution
}
