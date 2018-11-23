package com.animatinator.scala.learn.tunnelproblem

import org.scalatest.FunSuite

class TunnelProblemTest extends FunSuite {
  test("tunnelProblem_noTunnel") {
    assert(TunnelProblem.numberOfFittedObjects(Nil, List(1, 2, 3, 4, 5)) == 0)
  }

  test("tunnelProblem_noObjects") {
    assert(TunnelProblem.numberOfFittedObjects(List(1, 2, 3, 4, 5), Nil) == 0)
  }

  test("tunnelProblem_perfectFit") {
    assert(TunnelProblem.numberOfFittedObjects(List(5, 4, 3, 2, 1), List(5, 4, 3, 2, 1)) == 5)
  }

  test("tunnelProblem_perfectFit_reordered") {
    assert(TunnelProblem.numberOfFittedObjects(List(5, 4, 3, 2, 1), List(1, 4, 5, 3, 2)) == 5)
  }

  test("tunnelProblem_stuckAtEntrance") {
    assert(TunnelProblem.numberOfFittedObjects(List(1, 4, 3, 2, 1), List(2, 3, 4, 5)) == 0)
  }

  test("tunnelProblem_example") {
    assert(TunnelProblem.numberOfFittedObjects(List(3, 4, 2, 3), List(5, 1, 3, 3, 4)) == 3)
  }

  test("tunnelProblem_anotherExample") {
    assert(TunnelProblem.numberOfFittedObjects(List(6, 5, 6, 4, 5), List(6, 5, 6, 4, 5)) == 4)
  }
}
