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

  test("isSymmetric_ignoresNodeValues") {
    val tree =Node("x",
      Node("a",
        Node("b"),
        Node("c")),
      Node("d",
        Node("e"),
        Node("f")))
    assert(tree.isSymmetric)
  }

  test("addValue_empty") {
    assert(End.addValue("x") == Node("x"))
  }

  test("addValue_rightSide") {
    assert(Node(2).addValue(1) == Node(2, Node(1), End))
    assert(Node(2).addValue(3) == Node(2, End, Node(3)))
  }

  test("addValue_multiple") {
    val tree = Node(5).addValue(3).addValue(1).addValue(4).addValue(7).addValue(6).addValue(8)
    assert(tree ==
      Node(5,
        Node(3,
          Node(1),
          Node(4)),
        Node(7,
          Node(6),
          Node(8))))
  }

  test("fromList_multiple") {
    val tree = Tree.fromList(List(5, 3, 1, 4, 7, 6, 8))
    assert(tree ==
      Node(5,
        Node(3,
          Node(1),
          Node(4)),
        Node(7,
          Node(6),
          Node(8))))
  }

  test("fromList_symmetric") {
    assert(Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric)
  }

  test("fromList_notSymmetric") {
    assert(!Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric)
  }

  test("symmetricBalancedTrees_example") {
    val possibleTrees : List[Tree[String]] = Tree.symmetricBalancedTrees(5, "x")
    assert(possibleTrees.toString() ==
      "List(T(x T(x . T(x . .)) T(x T(x . .) .)), T(x T(x T(x . .) .) T(x . T(x . .))))")
  }

  test("hbalTrees_example") {
    val possibleTrees = Tree.hbalTrees(3, "x")
    assert(possibleTrees.length == 15)
    // Yeah, I know, but this is what was listed (http://aperiodic.net/phil/scala/s-99/ P59) and I don't feel like
    // working out exactly what the fifteen possibilities should be.
    assert(possibleTrees.toString startsWith
      "List(T(x T(x T(x . .) T(x . .)) T(x T(x . .) T(x . .))), T(x T(x T(x . .) T(x . .)) T(x T(x . .) .)),")
  }

  test("minHbalNodes_example") {
    assert(Tree.minHbalNodes(3) == 4)
  }

  test("maxHbalHeight_example") {
    assert(Tree.maxHbalHeight(4) == 3)
  }

  test("minHbalHeight_boundary_2_3") {
    assert(Tree.minHbalHeight(3) == 2)
    assert(Tree.minHbalHeight(4) == 3)
  }

  test("minHbalHeight_boundary_3_4") {
    assert(Tree.minHbalHeight(7) == 3)
    assert(Tree.minHbalHeight(8) == 4)
  }

  test("size_empty") {
    assert(End.size == 0)
  }

  test("size_singleNode") {
    assert(Node("x").size == 1)
  }

  test("size_fewNodes") {
    assert(Node("x", Node("a"), Node("b")).size == 3)
  }

  test("hbalTreesWithNodes_3") {
    assert(Tree.hbalTreesWithNodes(3, "x").length == 1)
  }

  test("hbalTreesWithNodes_5") {
    assert(Tree.hbalTreesWithNodes(5, "x").length == 6)
  }

  test("hbalTreesWithNodes_15") {
    assert(Tree.hbalTreesWithNodes(15, "x").length == 1553)
  }
}
