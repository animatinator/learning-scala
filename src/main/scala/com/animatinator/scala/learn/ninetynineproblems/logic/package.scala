package com.animatinator.scala.learn.ninetynineproblems

import scala.collection.mutable

package object logic {
  class EnhancedBoolean(x : Boolean) {
    def and(y : Boolean): Boolean = Logic.and(x, y)
    def or(y : Boolean): Boolean = Logic.or(x, y)
    def nand(y : Boolean): Boolean = Logic.nand(x, y)
    def nor(y : Boolean): Boolean = Logic.nor(x, y)
    def equ(y : Boolean): Boolean = Logic.equ(x, y)
    def xor(y : Boolean): Boolean = Logic.xor(x, y)
    def impl(y : Boolean): Boolean = Logic.impl(x, y)
  }

  object EnhancedBoolean {
    implicit def enhance(b : Boolean) : EnhancedBoolean = new EnhancedBoolean(b)
  }

  object Logic {
    //noinspection SimplifyBooleanMatch
    def not(b : Boolean): Boolean = b match {
      case true => false
      case false => true
    }

    def and(x : Boolean, y : Boolean): Boolean = (x, y) match {
      case (true, true) => true
      case _ => false
    }

    def or(x : Boolean, y : Boolean): Boolean = (x, y) match {
      case (false, false) => false
      case _ => true
    }

    def nand(x : Boolean, y : Boolean): Boolean = not(and(x, y))

    def nor(x : Boolean, y : Boolean): Boolean = not(or(x, y))

    def equ(x : Boolean, y : Boolean): Boolean = or(and(x, y), and(not(x), not(y)))

    def xor(x : Boolean, y : Boolean): Boolean = not(equ(x, y))

    def impl(x : Boolean, y : Boolean): Boolean = or(not(x), y)

    def table2(f : (Boolean, Boolean) => Boolean): Unit = {
      println("A\t\tB\t\tresult")
      for (a <- List(false, true); b <- List(false, true)) {
        println("%s\t%s\t%s".format(a, b, f(a, b)))
      }
    }

    def gray(n : Int) : List[List[Boolean]] = n match {
      case 1 => List(List(false), List(true))
      case _ =>
        val inner = gray(n - 1)
        (inner map {false :: _}) ::: (inner.reverse map {true :: _})
    }

    abstract sealed class HuffmanTree extends Ordered[HuffmanTree] {
      def frequency : Int
      override def compare(that: HuffmanTree): Int = that.frequency - this.frequency
    }

    case class HuffmanLeaf(value : String, frequency : Int) extends HuffmanTree

    case class HuffmanNode(left : HuffmanTree, right : HuffmanTree) extends HuffmanTree {
      override def frequency: Int = left. frequency + right.frequency
    }

    def toHuffmanPriorityQueue(input : List[(String, Int)]) : mutable.PriorityQueue[HuffmanTree] = {
      val result = mutable.PriorityQueue[HuffmanTree]()
      input map { value => HuffmanLeaf(value._1, value._2)} foreach {result.enqueue(_)}
      result
    }

    def huffman(input : List[(String, Int)]) : List[(String, String)] = {
      def buildHuffmanTree(queue : mutable.PriorityQueue[HuffmanTree]) : HuffmanTree = {
        if (queue.length == 1) queue.dequeue()
        else {
          queue.enqueue(HuffmanNode(queue.dequeue(), queue.dequeue()))
          buildHuffmanTree(queue)
        }
      }

      def generateCodesFromTree(tree : HuffmanTree) : List[(String, String)] = {
        def generateInner(tree : HuffmanTree, currentCode : String) : List[(String, String)] = tree match {
          case HuffmanLeaf(v, _) => List((v, currentCode))
          case HuffmanNode(t1, t2) => generateInner(t1, currentCode + "0") ::: generateInner(t2, currentCode + "1")
        }

        generateInner(tree, "")
      }

      generateCodesFromTree(buildHuffmanTree(toHuffmanPriorityQueue(input))) sortWith {_._1 < _._1}
    }
  }
}
