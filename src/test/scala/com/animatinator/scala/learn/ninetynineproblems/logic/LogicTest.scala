package com.animatinator.scala.learn.ninetynineproblems.logic

import org.scalatest.FunSuite
import EnhancedBoolean._

//noinspection NameBooleanParameters
class LogicTest extends FunSuite {
  test("not") {
    assert(Logic.not(false))
    assert(!Logic.not(true))
  }

  test("and") {
    assert(Logic.and(true, true))
    assert(!Logic.and(true, false))
    assert(!Logic.and(false, true))
    assert(!Logic.and(false, false))
  }

  test("or") {
    assert(Logic.or(true, true))
    assert(Logic.or(true, false))
    assert(Logic.or(false, true))
    assert(!Logic.or(false, false))
  }

  test("nand") {
    assert(!Logic.nand(true, true))
    assert(Logic.nand(true, false))
    assert(Logic.nand(false, true))
    assert(Logic.nand(false, false))
  }

  test("nor") {
    assert(!Logic.nor(true, true))
    assert(!Logic.nor(true, false))
    assert(!Logic.nor(false, true))
    assert(Logic.nor(false, false))
  }

  test("equ") {
    assert(Logic.equ(true, true))
    assert(!Logic.equ(true, false))
    assert(!Logic.equ(false, true))
    assert(Logic.equ(false, false))
  }

  test("xor") {
    assert(!Logic.xor(true, true))
    assert(Logic.xor(true, false))
    assert(Logic.xor(false, true))
    assert(!Logic.xor(false, false))
  }

  test("impl") {
    assert(Logic.impl(true, true))
    assert(!Logic.impl(true, false))
    assert(Logic.impl(false, true))
    assert(Logic.impl(false, false))
  }

  test("table2") {
    println("Table for AND:")
    Logic.table2(Logic.and)
    println
  }

  test("table2_enhanced") {
    println("Table for a more complex expression:")
    Logic.table2((a: Boolean, b: Boolean) => a and (a or Logic.not(b)))
    println
  }

  test("gray_1") {
    assert(Logic.gray(1) == List(List(false), List(true)))
  }

  test("gray_2") {
    assert(Logic.gray(2) == List(List(false, false), List(false, true), List(true, true), List(true, false)))
  }
}
