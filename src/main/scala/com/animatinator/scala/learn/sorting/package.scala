package com.animatinator.scala.learn

package object sorting {
  object MergeSort {
    private def merge[T](a: List[T], b: List[T])(implicit ord: T => Ordered[T]): List[T] = (a, b) match {
      case (a1 :: at, b1 :: bt) => if (a1 < b1) a1 :: b1 :: merge(at, bt) else b1 :: a1 :: merge(at, bt)
      case (as, Nil) => as
      case (Nil, bs) => bs
    }

    def mergeSort[T](list : List[T])(implicit ord: T => Ordered[T]) : List[T] = list match {
      case Nil => Nil
      case h :: Nil => List(h)
      case _ => merge(mergeSort(list.slice(0, list.length / 2)), mergeSort(list.slice(list.length / 2, list.length)))
    }
  }
}
