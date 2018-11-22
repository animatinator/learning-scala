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

  test("gcd_equal") {
    assert(ArithmeticProblems.gcd(7, 7) == 7)
  }

  test("gcd_greatest") {
    assert(ArithmeticProblems.gcd(8, 4) == 4)
  }

  test("gcd_trickier") {
    assert(ArithmeticProblems.gcd(36, 40) == 4)
  }

  test("gcd_coprime") {
    assert(ArithmeticProblems.gcd(37, 40) == 1)
  }

  test("gcd_secondMultipleOfFirst") {
    assert(ArithmeticProblems.gcd(4, 8) == 4)
  }

  test("isCoprimeTo_notCoprime") {
    assert(!8.isCoPrimeTo(4))
  }

  test("isCoprimeTo_coprime") {
    assert(9.isCoPrimeTo(4))
  }

  test("totient_example") {
    assert(10.totient == 4)
  }

  test("totient_prime") {
    assert(11.totient == 10)
  }

  test("primeFactors_none") {
    assert(7.primeFactors == List(7))
  }

  test("primeFactors_315") {
    assert(315.primeFactors == List(3, 3, 5, 7))
  }

  test("primeFactorMultiplicity_315") {
    assert(315.primeFactorsMultiplicity == List((3, 2), (5, 1), (7, 1)))
  }

  test("phi_example") {
    assert(10.phi == 4)
  }

  test("phi_prime") {
    assert(11.phi == 10)
  }

  def time[A](label: String)(block: => A): A = {
    val now = System.currentTimeMillis()
    val ret = block
    println(label + ": " + (System.currentTimeMillis() - now) + " ms.")
    ret
  }

  test("totient_comparison") {
    time("totient(1090)") {
      println(10900.totient)
    }
    time("phi(1090)") {
      println(10900.phi)
    }
  }

  test("listPrimesInRange") {
    assert(ArithmeticProblems.listPrimesInRange(7 to 31) == List(7, 11, 13, 17, 19, 23, 29, 31))
  }
}
