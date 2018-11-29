package com.animatinator.scala.learn.ninetynineproblems.graph

import org.scalatest.FunSuite

class GraphTest extends FunSuite {
  val termFormExample: (List[Char], List[(Char, Char, Int)]) =
    (List('k', 'm', 'p', 'q'), List(('m', 'q', 7), ('p', 'm', 5), ('p', 'q', 9)))
  val adjacencyFormExample: List[(Char, List[(Char, Int)])] =
    List(('k', Nil), ('m', List(('q', 7))), ('p', List(('m', 5), ('q', 9))), ('q', Nil))

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
    val graph = Graph.fromStringLabel("[b-c, f-c, g-h, d, f-b, k-f, h-g]")
    assertRepsEqual(
      graph.toTermForm,
      (List("d", "k", "h", "c", "f", "g", "b"),
        List(("h","g",1), ("k","f",1), ("f","b",1), ("g","h",1), ("f","c",1), ("b","c",1))))
  }

  test("fromStringLabel_example2") {
    val digraph = Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]")
    assertEqual(
      digraph.toAdjacentForm, List(("m",List(("q",7))), ("p",List(("m",5), ("q",9))), ("k",List()), ("q",List())))
  }

  def assertRepsEqual[T, U](first : (List[T], List[U]), second : (List[T], List[U])): Boolean =
    assertEqual(first._1, second._1) && assertEqual(first._2, second._2)

  def assertEqual[T](first : List[T], second : List[T]): Boolean = (first diff second) == Nil
}
