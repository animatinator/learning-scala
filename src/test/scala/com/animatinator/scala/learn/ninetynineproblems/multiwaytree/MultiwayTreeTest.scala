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

  test("internalPathLength_singleNode") {
    assert(MTree("x").internalPathLength == 0)
  }

  test("internalPathLength_someNodes") {
    val test : MTree[Char] = "ab^c^d^^"
    assert(test.internalPathLength == 3)
  }

  test("internalPathLength_exampleTree") {
    assert(exampleTree.internalPathLength == 9)
  }

  test("postorder_singleNode") {
    assert(MTree("x").postorder == List("x"))
  }

  test("postorder_exampleTree") {
    assert(exampleTree.postorder == List('g', 'f', 'c', 'd', 'e', 'b', 'a'))
  }
}
