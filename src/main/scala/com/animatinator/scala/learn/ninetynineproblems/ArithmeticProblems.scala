package com.animatinator.scala.learn.ninetynineproblems

class S99Int(val start: Int) {
  import S99Int._

  def isPrime : Boolean = start > 1 && (primes takeWhile {_ <= Math.sqrt(start)} forall {start % _ != 0})

  def isCoPrimeTo(other : Int) : Boolean = gcd(start, other) == 1

  def totient: Int = (1 to start) count { x => x isCoPrimeTo start}

  def primeFactors: List[Int] = {
    def primeFactorsInner(rem : Int, primes : Stream[Int]) : List[Int] = {
      if (rem.isPrime) List(rem)
      else if (rem % primes.head == 0) primes.head :: primeFactorsInner(rem / primes.head, primes)
      else primeFactorsInner(rem, primes.tail)
    }

    primeFactorsInner(start, primes)
  }

  def primeFactorsMultiplicity : List[(Int, Int)] = ListProblems.encode(primeFactors) map {_.swap}

  def phi : Int = start.primeFactorsMultiplicity.foldLeft(1) {
    (r, f) => f match {case (factor, mult) => r * (factor - 1) * Math.pow(factor, mult- 1).toInt}
  }

  def goldbach : (Int , Int) = {
    val primes = listPrimesInRange(1 to start)
    for (x <- primes; y <- primes if y > x) if (x + y == start) return (x, y)
    throw new IllegalArgumentException
  }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })

  def gcd(x : Int, y : Int) : Int = x % y match {
    case 0 => y
    case n => gcd(y, n)
  }

  def listPrimesInRange(range : Range) : List[Int] =
    S99Int.primes dropWhile(_ < range.start) takeWhile(_ <= range.end) toList

  def printGoldbachList(r : Range): Unit = {
    r filter (_ % 2 == 0) foreach {x =>
      val (l, r) = x.goldbach
      println("%s = %s + %s".format(x, l, r))
    }
  }
}
