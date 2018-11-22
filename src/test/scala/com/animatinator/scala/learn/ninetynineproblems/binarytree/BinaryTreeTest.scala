package com.animatinator.scala.learn.ninetynineproblems.binarytree

import org.scalatest.FunSuite


class BinaryTreeTest extends FunSuite {
  test("cbalanced_singleNode") {
    assert(Tree.cBalanced(1, "x") == List(Node("x")))
  }

  test("cbalanced_threeNodes") {
    assert(Tree.cBalanced(3, "x") == List(Node("x", Node("x"), Node("x"))))
  }

  test("cbalanced_unbalancedWithFourNodes") {
    val options : List[Tree[String]] = Tree.cBalanced(4, "x")
    assert(options == List(
      Node("x", Node("x"), Node("x", End, Node("x"))),
      Node("x", Node("x", End, Node("x")), Node("x")),
      Node("x", Node("x"), Node("x", Node("x"), End)),
      Node("x", Node("x", Node("x"), End), Node("x"))
    ))
  }
}
