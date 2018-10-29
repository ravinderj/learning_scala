package com.ravinder

import com.ravinder.List.map
import com.ravinder.List.filter
import com.ravinder.List.reduce
import org.scalatest.{FunSuite, Matchers}

class ListTest extends FunSuite with Matchers {
  private def doubleIt(a: Int): Int = 2 * a

  private def isEven(num: Int): Boolean = num % 2 == 0

  private def isOdd(num: Int): Boolean = num % 2 != 0

  private def sum(a: Int, b: Int): Int = a + b

  private def multiply(a: Int, b: Int): Int = a * b

  test("should create list from given numbers") {
    val list = List(1, 2)

    list shouldBe Cons(1, Cons(2, Nil))
  }

  test("should give nil when no argument is passed") {
    val list = List.empty()

    list shouldBe Nil
  }

  test("should double all the numbers of a list when the mapper function is to double the number") {
    val list = List(1, 2, 3, 4)

    val actual = map(list, doubleIt)

    val expected = List(2, 4, 6, 8)

    actual shouldBe expected
  }

  test("should give Nil for map on empty list") {
    map(List(), doubleIt) shouldBe List()
  }

  test("should filter out even numbers from given list") {
    val list = List(1, 2, 3, 4)
    val expected = List(2, 4)

    val actual = filter(list, isEven)
    actual shouldBe expected
  }

  test("should filter out odd numbers from given list") {
    val list = List(1, 2, 3, 4)
    val expected = List(1, 3)

    val actual = filter(list, isOdd)
    actual shouldBe expected
  }

  test("should return Nil for an empty list") {
    val list = List.empty()
    val expected = List()

    val actualOnCallingIsEven = filter(list, isEven)
    val actualOnCallingIsOdd = filter(list, isOdd)
    actualOnCallingIsEven shouldBe expected
    actualOnCallingIsOdd shouldBe expected
  }

  test("should add all elements when reducer function is to sum two elements") {
    val list = List(1, 2, 3, 4)
    val expected = 10

    val actual = reduce(list, sum, 0)
    actual shouldBe expected
  }

  test("should get multiplication of all numbers when reducer function is to multiply two numbers") {
    val list = List(1, 2, 3, 4)
    val expected = 24

    val actual = reduce(list, multiply, 1)
    actual shouldBe expected
  }

  test("should return initial value when array is empty") {
    val list = List.empty()
    val expected = 0

    val actual = reduce(list, sum, 0)
    actual shouldBe expected
  }
}
