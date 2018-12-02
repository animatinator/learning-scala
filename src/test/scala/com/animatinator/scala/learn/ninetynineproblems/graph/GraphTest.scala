package com.animatinator.scala.learn.ninetynineproblems.graph

import org.scalatest.FunSuite

class GraphTest extends FunSuite {
  val termFormExample: (List[Char], List[(Char, Char, Int)]) =
    (List('k', 'm', 'p', 'q'), List(('m', 'q', 7), ('p', 'm', 5), ('p', 'q', 9)))
  val adjacencyFormExample: List[(Char, List[(Char, Int)])] =
    List(('k', Nil), ('m', List(('q', 7))), ('p', List(('m', 5), ('q', 9))), ('q', Nil))

  val exampleGraph: Graph[Char, Int] = Graph.termLabel(termFormExample._1, termFormExample._2)
  val tree: Graph[String, Int] = Graph.fromString("[a-b, a-c, b-d, b-e, c-f, c-g]")

  test("toTermForm_singleNode") {
    assert(Digraph.term(List('a'), Nil).toTermForm == (List('a'), Nil))
    assert(Graph.term(List('a'), Nil).toTermForm == (List('a'), Nil))
  }

  test("toTermForm_example") {
    val digraph = Digraph.termLabel(termFormExample._1, termFormExample._2)
    assertRepsEqual(digraph.toTermForm, termFormExample)
    val graph = Graph.termLabel(termFormExample._1, termFormExample._2)
    assertRepsEqual(graph.toTermForm, termFormExample)
  }

  test("toAdjacentForm_singleNode") {
    assert(Digraph.adjacent(List(('a', Nil))).toAdjacentForm == List(('a', Nil)))
    assert(Graph.adjacent(List(('a', Nil))).toAdjacentForm == List(('a', Nil)))
  }

  test("toAdjacentForm_example") {
    val digraph = Digraph.adjacentLabel(adjacencyFormExample)
    assertEqual(digraph.toAdjacentForm, adjacencyFormExample)
    val graph = Graph.adjacentLabel(adjacencyFormExample)
    assertEqual(graph.toAdjacentForm, adjacencyFormExample)
  }

  test("toString_singleNode") {
    assert(Digraph.adjacentLabel(List(("a", Nil))).toString == "[a]")
    assert(Graph.adjacentLabel(List(("a", Nil))).toString == "[a]")
  }

  test("toString_twoNodes") {
    assert(Digraph.adjacentLabel(List(("a", List(("b", 7))), ("b", Nil))).toString == "[a>b/7]")
    assert(Graph.adjacentLabel(List(("a", List(("b", 7))), ("b", Nil))).toString == "[a-b/7]")
  }

  test("toString_example") {
    val digraph = Digraph.adjacentLabel(adjacencyFormExample)
    assert(digraph.toString == "[k, m>q/7, p>m/5, p>q/9]")
    val graph = Graph.adjacentLabel(adjacencyFormExample)
    assert(graph.toString == "[k, m-q/7, p-m/5, p-q/9]")
  }

  test("fromStringLabel_example1") {
    val graph = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]")
    assertRepsEqual(
      graph.toTermForm,
      (List("d", "k", "h", "c", "f", "g", "b"),
        List(("h","g",1), ("k","f",1), ("f","b",1), ("g","h",1), ("f","c",1), ("b","c",1))))
  }

  test("fromStringLabel_example2") {
    val digraph = Digraph.fromString("[p>q/9, m>q/7, k, p>m/5]")
    assertEqual(
      digraph.toAdjacentForm, List(("m",List(("q",7))), ("p",List(("m",5), ("q",9))), ("k",List()), ("q",List())))
  }

  test("findPaths_example") {
    assert(
      Digraph.fromString("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q")
        == List(List("p", "q"), List("p", "m", "q")))
  }

  test("findPaths_noPathsExample") {
    assert(Digraph.fromString("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "k") == Nil)
  }

  test("findCycles_example") {
    assert(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles("f")
      == List(List("f", "c", "b", "f"), List("f", "b", "c", "f")))
  }

  test("equals") {
    assert(Graph.fromString("[b-c/1, a-b/1]") == Graph.fromString("[a-b/1, b-c/1]"))
  }

  test("spanningTrees_example") {
    val graph = Graph.term(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
      List(('a', 'b'), ('a', 'd'), ('b', 'c'), ('b', 'e'),
        ('c', 'e'), ('d', 'e'), ('d', 'f'), ('d', 'g'),
        ('e', 'h'), ('f', 'g'), ('g', 'h')))

    // Expensive!
    //println(graph.spanningTrees.length)

    assert(Graph.fromString("[a-b, b-c, a-c]").spanningTrees.length == 3)
  }

  test("isTree_notATree") {
    assert(!exampleGraph.isTree)
  }

  test("isTree_simpleTree") {
    assert(tree.isTree)
  }

  test("isConnected_notConnected") {
    assert(!exampleGraph.isConnected)
  }

  test("isConnected_tree") {
    assert(tree.isConnected)
  }

  test("minimumSpanningTree_notConnected") {
    assertThrows[Exception](exampleGraph.minimumSpanningTree)
  }

  test("minimumSpanningTree_simpleTree") {
    assert(tree.minimumSpanningTree == tree)
  }

  test("minimumSpanningTree_example") {
    assert(Graph.fromString("[a-b/1, b-c/2, a-c/3]").minimumSpanningTree == Graph.fromString("[a-b/1, b-c/2]"))
  }

  test("minimumSpanningTree_biggerExample") {
    val graph = Graph.termLabel(
      List("a", "b", "c", "d", "e", "f", "g", "h"),
      List(("a", "b", 5), ("a", "d", 3), ("b", "c", 2), ("b", "e", 4),
        ("c", "e", 6), ("d", "e", 7), ("d", "f", 4), ("d", "g", 3),
        ("e", "h", 5), ("f", "g", 4), ("g", "h", 1)))

    assert(graph.minimumSpanningTree == Graph.fromString("[f-g/4, a-d/3, d-g/3, g-h/1, e-h/5, b-c/2, b-e/4]"))
  }

  test("possibleMappings_empty") {
    assert(Graph.possibleMappings(List(), List()) == List(Map()))
    assert(Graph.possibleMappings(List(), List(1)) == List(Map()))
    assert(Graph.possibleMappings(List(1), List()) == Nil)
  }

  test("possibleMappings_simple") {
    assert(Graph.possibleMappings(List("a", "b"), List(1, 2))
      == List(Map("a" -> 1, "b" -> 2), Map("a" -> 2, "b" -> 1)))
  }

  test("applyMapping_numerify") {
    assert(Graph.fromString("[a-b, b-c, c-d]").applyMapping(Map("a" -> "1", "b" -> "2", "c" -> "3", "d" -> "4"))
      == Graph.fromString("[1-2, 2-3, 3-4]"))
  }

  test("isIsomorphicTo_singleNode") {
    assert(Graph.fromString("[a]").isIsomorphicTo(Graph.fromString("[t]")))
  }

  test("isIsomorphicTo_simpleExample") {
    assert(Graph.fromString("[a-b]").isIsomorphicTo(Graph.fromString("[5-7]")))
  }

  test("isIsomorphicTo_moreComplex") {
    val g1 = Graph.fromString("[a-b, b-c, c-d, a-d, a-c]")
    val g2 = Graph.fromString("[c-a, a-t, t-s, c-s, c-t]")
    assert(g1.isIsomorphicTo(g2))
  }

  def assertRepsEqual[T, U](first : (List[T], List[U]), second : (List[T], List[U])): Boolean =
    assertEqual(first._1, second._1) && assertEqual(first._2, second._2)

  def assertEqual[T](first : List[T], second : List[T]): Boolean = (first diff second) == Nil
}
