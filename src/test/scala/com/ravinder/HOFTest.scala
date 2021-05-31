package com.ravinder

import org.scalatest.{FunSuite, Matchers}

class HOFTest extends FunSuite with Matchers {
  private def add2Nums(a: Int, b: Int): Int = a + b

  private def add5(n: Int): Int = n + 5

  private def multiplyBy3(n: Int): Int = n * 3

  test("partial1 takes a value and a function of two arguments and returns a function of one argument") {
    val add5 = HOF.partial1(5, add2Nums)
    add5(10) shouldBe 15
  }

  test("curry takes a function of N arguments and returns a function of one argument that returns another function") {
    val curriedAdd = HOF.curry(add2Nums)
    curriedAdd(5)(10) shouldBe 15
  }

  test("uncurry should reverse the tranformation of curry") {
    val curriedAdd = HOF.curry(add2Nums)
    val uncurriedAdd = HOF.uncurry(curriedAdd)
    uncurriedAdd(5, 10) shouldBe 15
  }

  test("compose should feed output of one function to another as input") {
    HOF.compose(multiplyBy3, add5)(5) shouldBe 30
  }
}
