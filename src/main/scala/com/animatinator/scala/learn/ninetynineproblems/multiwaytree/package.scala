package com.animatinator.scala.learn.ninetynineproblems

package object multiwaytree {

  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def this(value: T) = this(value, List())

    def nodeCount : Int = 1 + children.map(_.nodeCount).sum

    override def toString : String = value + children.map(_.toString).mkString + "^"

    def toStringOld: String = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
  }

  object MTree {
    def apply[T](value: T) = new MTree(value, List())
    def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)

    implicit def stringToMTree(string : String): MTree[Char] = {
      def stringToMTreeInner(string : String, index : Int) : (MTree[Char], Int) = {
        val nodeValue = string(index)
        var curIndex = index + 1
        var subtrees : List[MTree[Char]] = List()
        while (curIndex < string.length && string(curIndex) != '^') {
          val (newTree, newCurIndex) = stringToMTreeInner(string, curIndex)
          curIndex = newCurIndex
          subtrees ::= newTree
        }
        (MTree(nodeValue, subtrees.reverse), curIndex + 1)
      }
      stringToMTreeInner(string, 0)._1
    }
  }
}
