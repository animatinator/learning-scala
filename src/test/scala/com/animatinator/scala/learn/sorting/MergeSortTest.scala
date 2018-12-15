package com.animatinator.scala.learn.sorting

import org.scalatest.FunSuite

class MergeSortTest extends FunSuite {
  test("emptyList") {
    assert(MergeSort.mergeSort(Nil) == Nil)
  }

  test("singleElement") {
    assert(MergeSort.mergeSort(List(1)) == List(1))
  }

  test("someList") {
    assert(MergeSort.mergeSort(List(5, 4, 7, 2, 8, 5, 1, 2, 0, 8, 9)) == List(0, 2, 1, 4, 5, 5, 7, 8, 2, 8, 9))
  }
}
