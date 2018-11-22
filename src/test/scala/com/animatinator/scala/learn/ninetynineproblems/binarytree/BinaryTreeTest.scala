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

  test("reflect_empty") {
    assert(End.reflect == End)
  }

  test("reflect_singleNode") {
    assert(Node("x").reflect == Node("x"))
  }

  test("reflect_complex") {
    val left = Node("a", Node("b"), Node("c"))
    val right = Node("a", Node("c"), Node("b"))
    assert(left.reflect == right)
  }

  test("isSymmetric_empty") {
    assert(End.isSymmetric)
  }

  test("isSymmetric_singleNode") {
    assert(Node("x").isSymmetric)
  }

  test("isSymmetric_biggerTree") {
    val tree =Node("x",
      Node("a",
        Node("b"),
        Node("c")),
      Node("a",
        Node("c"),
        Node("b")))
    assert(tree.isSymmetric)
  }

  test("isSymmetric_biggerTreeWithWrongNode") {
    val tree =Node("x",
      Node("a",
        Node("b"),
        Node("c",
          Node("d"),
          End)),
      Node("a",
        Node("c"),
        Node("b")))
    assert(!tree.isSymmetric)
  }
}
