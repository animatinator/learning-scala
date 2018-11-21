package com.animatinator.scala.learn.ninetynineproblems

import org.scalatest.FunSuite

class ArithmeticProblemsTest extends FunSuite {
  import S99Int._

  test("isPrime_primesList") {
    assert((S99Int.primes take 20).toList
      == List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71))
  }

  test("isPrime_obviouslyNotPrime") {
    assert(!360.isPrime)
  }

  test("isPrime_clearlyPrime") {
    assert(7.isPrime)
  }
}
