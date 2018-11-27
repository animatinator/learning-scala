package com.animatinator.scala.learn.ninetynineproblems.multiwaytree

import org.scalatest.FunSuite

class MultiwayTreeTest extends FunSuite {
  val exampleTree =
    MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))))
  val exampleTreeString = "afg^^c^bd^e^^^"

  test("nodeCount_singleNode") {
    assert(MTree("x").nodeCount == 1)
  }

  test("nodeCount_exampleTree") {
    assert(exampleTree.nodeCount == 7)
  }

  test("stringToMTree_exampleTree") {
    val test : MTree[Char] = exampleTreeString
    assert(test == exampleTree)
  }

  test("toString_exampleTree") {
    assert(exampleTree.toString == exampleTreeString)
  }
}
