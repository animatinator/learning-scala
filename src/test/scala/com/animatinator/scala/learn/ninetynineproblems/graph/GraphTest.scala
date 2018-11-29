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

  test("toString_example") {
    val digraph = Digraph.adjacentLabel(adjacencyFormExample)
    assert(digraph.toString == "[p>q/9, p>m/5, m>q/7, k]")
    val graph = Graph.adjacentLabel(adjacencyFormExample)
    assert(graph.toString == "[m-q/7, q-p/9, p-q/9, m-p/5, p-m/5, q-m]/7")
  }

  def assertRepsEqual[T, U](first : (List[T], List[U]), second : (List[T], List[U])): Boolean =
    assertEqual(first._1, second._1) && assertEqual(first._2, second._2)

  def assertEqual[T](first : List[T], second : List[T]): Boolean = (first diff second) == Nil
}
