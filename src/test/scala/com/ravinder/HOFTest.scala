package com.ravinder

import org.scalatest.{FunSuite, Matchers}

class HOFTest extends FunSuite with Matchers {
  private def add2Nums(a: Int, b: Int): Int = a + b

  test("partial1 takes a value and a function of two arguments and returns a function of one argument") {
    val add5 = HOF.partial1(5, add2Nums)
    add5(10) shouldBe 15
  }
}
