package com.animatinator.scala.learn

import org.scalatest.FunSuite

class PythagorasTest extends FunSuite {
  test("Pythagoras.pythagoras_simple") {
    assert(Pythagoras.hypotenuse(3.0, 4.0) === 5.0)
  }

  test("Pythagoras.pythagoras_onezero") {
    assert(Pythagoras.hypotenuse(3.0, 0.0) === 3.0)
  }

  test("Pythagoras.pythagoras_zero") {
    assert(Pythagoras.hypotenuse(0.0, 0.0) === 0.0)
  }

  test("Pythagoras.pythagoras_negative") {
    assert(Pythagoras.hypotenuse(-3.0, 4.0) === 5.0)
  }
}
