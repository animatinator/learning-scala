package com.animatinator.scala.learn.ninetynineproblems.util

class Parser(string: String) {
  var index : Int = 0

  def isEmpty : Boolean = index >= string.length

  def expectSymbol(symbol : Char): Unit = {
    if (string(index) != symbol)
      throw new IllegalStateException("Tried to read '%s' but found '%s'".format(symbol, string(index)))
    index += 1
  }

  def readChar : Char = {
    val result : Char = string(index)
    index += 1
    result
  }

  def peek : Char = string(index)
}

object Parser {
  implicit def toParser(string : String) : Parser = new Parser(string)
}