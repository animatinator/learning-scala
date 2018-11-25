package com.animatinator.scala.learn.ninetynineproblems.binarytree

import org.scalatest.{Assertion, FunSuite}


class BinaryTreeTest extends FunSuite {

  val smallishTestTree = Node('a', Node('b'), Node('c', Node('d'), Node('e')))

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
    assert(possibleTrees ==
      List(Node("x", Node("x", End, Node("x")), Node("x", Node("x"), End)),
      Node("x", Node("x", Node("x"), End), Node("x", End, Node("x")))))
  }

  test("hbalTrees_example") {
    val possibleTrees = Tree.hbalTrees(3, "x")
    assert(possibleTrees.length == 15)
    assert(possibleTrees.toString ==
      "List(x(x(x,x),x(x,x)), x(x(x,x),x(x,)), x(x(x,x),x(,x)), x(x(x,),x(x,x)), x(x(x,),x(x,)), x(x(x,),x(,x)), " +
        "x(x(,x),x(x,x)), x(x(,x),x(x,)), x(x(,x),x(,x)), x(x(x,x),x), x(x,x(x,x)), x(x(x,),x), x(x,x(x,)), " +
        "x(x(,x),x), x(x,x(,x)))")
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

  test("leafCount_empty") {
    assert(End.leafCount == 0)
  }

  test("leafCount_singleNode") {
    assert(Node("x").leafCount == 1)
  }

  test("leafCount_example") {
    assert(Node('x', Node('x'), End).leafCount == 1)
  }

  test("leafCount_bigger") {
    assert(Node('x', Node('x', Node("y"), Node("z")), Node("p")).leafCount == 3)
  }

  test("leafList_empty") {
    assert(End.leafList == Nil)
  }

  test("leafList_singleNode") {
    assert(Node("x").leafList == List("x"))
  }

  test("leafList_example") {
    assert(smallishTestTree.leafList == List('b', 'd', 'e'))
  }

  test("internalList_empty") {
    assert(End.internalList == Nil)
  }

  test("internalList_singleNode") {
    assert(Node("x").internalList == Nil)
  }

  test("internalList_example") {
    assert(smallishTestTree.internalList == List('a', 'c'))
  }

  test("atLevel_example") {
    assert(smallishTestTree.atLevel(1) == List('a'))
    assert(smallishTestTree.atLevel(2) == List('b', 'c'))
    assert(smallishTestTree.atLevel(3) == List('d', 'e'))
  }

  test("completeBinaryTree_empty") {
    assert(Tree.completeBinaryTree(0, "x") == End)
  }

  test("completeBinaryTree_singleNode") {
    assert(Tree.completeBinaryTree(1, "x") == Node("x"))
  }

  test("completeBinaryTree_threeNodes") {
    assert(Tree.completeBinaryTree(3, "x") == Node("x", Node("x"), Node("x")))
  }

  test("completeBinaryTree_fiveNodes") {
    assert(Tree.completeBinaryTree(5, "x") == Node("x", Node("x", Node("x"), Node("x")), Node("x")))
  }

  test("layoutBinaryTree_empty") {
    assert(End.layoutBinaryTree == End)
  }

  test("layoutBinaryTree_singleNode") {
    assert(Node("x").layoutBinaryTree == PositionedNode("x", End, End, 1, 1))
  }

  test("layoutBinaryTree_example") {
    assert(
      Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree.toString ==
        "T[3,1](a T[1,2](b . T[2,3](c . .)) T[4,2](d . .))")
  }

  test("layoutBinaryTree_complex") {
    val laidOutTree = Tree.fromList(List('n','k','m','c','a','h','g','e','u','p','s','q')).layoutBinaryTree
    // TODO: Doing a string comparison because normal comparison fails. Unclear why.
    assert(laidOutTree.toString ==
      PositionedNode("n",
        PositionedNode("k",
          PositionedNode("c",
            PositionedNode("a", End, End,
              1, 4),
            PositionedNode("h",
              PositionedNode("g",
                PositionedNode("e", End, End,
                  3, 6),
                End,
                4, 5),
              End,
              5, 4),
            2, 3),
          PositionedNode("m", End, End,
            7, 3),
          6, 2),
        PositionedNode("u",
          PositionedNode("p",
            End,
            PositionedNode("s",
              PositionedNode("q", End, End,
                10, 5),
              End,
              11, 4),
            9, 3),
          End,
          12, 2),
        8, 1).toString)
  }

  test("height_empty") {
    assert(End.height == 0)
  }

  test("height_singleNode") {
    assert(Node("x").height == 1)
  }

  test("height_example") {
    assert(smallishTestTree.height == 3)
  }

  test("heightOfLeftmostPoint_empty") {
    assert(End.heightOfLeftmostPoint == 0)
  }

  test("heightOfLeftmostPoint_singleNode") {
    assert(Node("x").heightOfLeftmostPoint == 1)
  }

  test("heightOfLeftmostPoint_example") {
    assert(smallishTestTree.heightOfLeftmostPoint == 2)
  }

  test("heightOfLeftmostPoint_bigRightwardsTree") {
    val sillyTree = Node("a", End, Node("b", End, Node("c", End, Node("d"))))
    assert(sillyTree.heightOfLeftmostPoint == 1)
  }

  test("layoutBinaryTree2_empty") {
    assert(End.layoutBinaryTree2 == End)
  }

  test("layoutBinaryTree2_singleNode") {
    assert(Node("x").layoutBinaryTree2 == PositionedNode("x", End, End, 1, 1))
  }

  test("layoutBinaryTree2_threeNodes") {
    assert(Node("x", Node("l"), Node("r")).layoutBinaryTree2 ==
      PositionedNode("x",
        PositionedNode("l", End, End, 1, 2),
        PositionedNode("r", End, End, 3, 2),
        2, 1))
  }

  test("layoutBinaryTree2_example") {
    val expectedSmallishTestTreeLayout =
      PositionedNode('a',
        PositionedNode('b', End, End, 1, 2),
        PositionedNode('c',
          PositionedNode('d', End, End, 4, 3),
          PositionedNode('e', End, End, 6, 3),
          5, 2),
        3, 1)
    assert(smallishTestTree.layoutBinaryTree2 == expectedSmallishTestTreeLayout)
  }

  val subTreeForLayoutTesting : Tree[Char] = Tree.fromList(List('k','m','c','a','e','d','g'))

  test("minimumDistanceBetweenTrees_example") {
    val testLeftBounds = List((0, 0), (-1, 1), (0, 2))
    val testRightBounds = List((0, 0))
    assert(Tree.minimumDistanceBetweenTreesWithBounds(testLeftBounds, testRightBounds) == 2)
  }

  test("minimumDistanceBetweenTrees_exampleWithStickyOutBits") {
    val testLeftBounds = List((0, 0), (-1, 1), (0, 2))
    val testRightBounds = List((0, 0), (-1, -1), (-2, 0))
    assert(Tree.minimumDistanceBetweenTreesWithBounds(testLeftBounds, testRightBounds) == 5)
  }

  test("minimumDistanceBetweenTrees_bigStickyOutBranches") {
    val testLeftBounds = List((0, 0), (1, 1), (2, 2), (3, 3))
    val testRightBounds = List((0, 0), (-1, -1), (-2, -2), (-3, -3))
    assert(Tree.minimumDistanceBetweenTreesWithBounds(testLeftBounds, testRightBounds) == 7)
  }

  test("bounds_empty") {
    assert(End.bounds == Nil)
  }

  test("bounds_singleNode") {
    assert(Node("x").bounds == List((0, 0)))
  }

  test("bounds_testTree") {
    assert(smallishTestTree.bounds == List((0, 0), (-1, 1), (0, 2)))
  }

  test("bounds_listStyleTree") {
    val listStyleTree = Node("a", Node("b", Node("c"), End), End)
    assert(listStyleTree.bounds == List((0, 0), (-1, -1), (-2, -2)))
  }

  test("bounds_zigZagTree") {
    val listStyleTree = Node("a", Node("b", End, Node("c")), End)
    assert(listStyleTree.bounds == List((0, 0), (-1, -1), (0, 0)))
  }

  test("layoutBinaryTree3_empty") {
    assert(End.layoutBinaryTree3 == End)
  }

  test("layoutBinaryTree3_singleNode") {
    assert(Node("x").layoutBinaryTree3 == PositionedNode("x", End, End, 1, 1))
  }

  test("layoutBinaryTree3_testTree") {
    val smallishTreeLayout = smallishTestTree.layoutBinaryTree3
    assertMatchesPositions(smallishTreeLayout, Map('a' -> (2, 1), 'b' -> (1, 2), 'c' -> (3, 2), 'd' -> (2, 3), 'e' -> (4, 3)))
  }

  test("layoutBinaryTree3_example") {
    val exampleTree = Tree.fromList(List('n','k','m','c','a','e','d','g','u','p','q'))
    assert(exampleTree.bounds == List((0, 0), (-2, 2), (-3, 1), (-4, 2), (-3, -1)))
    assertMatchesPositions(
      exampleTree.layoutBinaryTree3,
      Map('n' -> (5,1), 'k' -> (3, 2), 'm' -> (4, 3), 'c' -> (2, 3), 'a' -> (1, 4), 'e' -> (3, 4), 'd' -> (2, 5),
        'g' -> (4, 5), 'u' -> (7, 2), 'p' -> (6, 3), 'q' -> (7, 4)))
  }

  def assertMatchesPositions[T](tree: Tree[T], positionMap : Map[T, (Int, Int)]) : Unit = tree match {
    case PositionedNode(value, l, r, x, y) =>
      assertOptionEqualsIfPresent(positionMap.get(value), (x, y))
      assertMatchesPositions(l, positionMap)
      assertMatchesPositions(r, positionMap)
    case End =>
  }

  def assertOptionEqualsIfPresent[T](option : Option[T], value : T): Assertion =
    assert(option.isEmpty || option.get == value)

  test("toString_empty") {
    assert(End.toString == "")
  }

  test("toString_singleNode") {
    assert(Node("x").toString == "x")
  }

  test("toString_testTree") {
    assert(smallishTestTree.toString == "a(b,c(d,e))")
  }
}
