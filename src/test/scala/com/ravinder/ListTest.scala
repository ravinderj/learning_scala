package com.ravinder

import org.scalatest.{FunSuite, Matchers}

class ListTest extends FunSuite with Matchers {
  private def doubleIt(a: Int): Int = 2 * a

  private def isEven(num: Int): Boolean = num % 2 == 0

  private def isOdd(num: Int): Boolean = num % 2 != 0

  private def sum(a: Int, b: Int): Int = a + b

  private def multiply(a: Int, b: Int): Int = a * b

  test("should create list with given numbers") {
    val list = List(1, 2)

    list shouldBe Cons(1, Cons(2, Nil))
  }

  test("empty list is Nil") {
    List() shouldBe Nil

    List.empty shouldBe Nil
  }

  test("should double all the numbers of a list when the mapper function is to double the number") {
    val list = List(1, 2, 3, 4)

    val actual = list.map(doubleIt)

    val expected = List(2, 4, 6, 8)

    actual shouldBe expected
  }

  test("should give Nil for map on empty list") {
    List.empty.map(doubleIt) shouldBe Nil
  }

  test("should filter out even numbers from given list") {
    val list = List(1, 2, 3, 4)
    val expected = List(2, 4)

    val actual = list.filter(isEven)
    actual shouldBe expected
  }

  test("should filter odd numbers from given list") {
    val list = List(1, 2, 3, 4)
    val expected = List(1, 3)

    val actual = list.filter(isOdd)
    actual shouldBe expected
  }

  test("filter should return Nil for an empty list") {
    val list = List.empty
    val expected = Nil

    val actualOnCallingIsEven = list.filter(isEven)
    val actualOnCallingIsOdd = list.filter(isOdd)
    actualOnCallingIsEven shouldBe expected
    actualOnCallingIsOdd shouldBe expected
  }

  test("should add all numbers when reducer function is to sum two numbers") {
    val list = List(1, 2, 3, 4)
    val expected = 10

    val actual = list.reduce(0)(sum)
    actual shouldBe expected
  }

  test("should get multiplication of all numbers when reducer function is to multiply two numbers") {
    val list = List(1, 2, 3, 4)
    val expected = 24

    val actual = list.reduce(1)(multiply)
    actual shouldBe expected
  }

  test("should return initial value when list is empty") {
    val list = List.empty
    val expected = 0

    val actual = list.reduce(0)(sum)
    actual shouldBe expected
  }

  test("should return reverse of list") {
    val list = List(1, 2, 3, 4)
    val expected = List(4, 3, 2, 1)

    val actual = list.reverse
    actual shouldBe expected
  }

  test("should return Nil for reverse of empty list") {
    List.empty.reverse shouldBe Nil
  }
}
