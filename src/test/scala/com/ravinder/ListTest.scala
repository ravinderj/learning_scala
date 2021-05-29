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

    val expected = List(2, 4, 6, 8)

    val actual = list.map(doubleIt)

    actual shouldBe expected
  }

  test("should give Nil for map on empty list") {
    List.empty.map(doubleIt) shouldBe Nil
  }

  test("should filter out even numbers from given list") {
    List(1, 2, 3, 4).filter(isEven) shouldBe List(2, 4)
  }

  test("should filter odd numbers from given list") {
    List(1, 2, 3, 4).filter(isOdd) shouldBe List(1, 3)
  }

  test("filter should return Nil for an empty list") {
    List.empty.filter(isEven) shouldBe Nil
    List.empty.filter(isOdd) shouldBe Nil
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
    List.empty.reduce(0)(sum) shouldBe 0
  }

  test("should return reverse of list") {
    List(1, 2, 3, 4).reverse shouldBe List(4, 3, 2, 1)
  }

  test("should return Nil for reverse of empty list") {
    List.empty.reverse shouldBe Nil
  }

  test("should flatmap the list") {
    val numbers = List(1, 4, 7, 10)
    val expected = List(0, 2, 3, 5, 6, 8, 9, 11)

    def oneAboveAndBelow(num: Int): List[Int] = List(num - 1, num + 1)

    val actual = numbers.flatMap(oneAboveAndBelow)
    actual shouldBe expected
  }

  test("should add two lists") {
    List(0, 1, 2) + List(4, 9) shouldBe List(0, 1, 2, 4, 9)
  }

  test("list should be sorted") {
    assert(List.isSorted(List(1, 2, 5, 100)))
    assert(List.isSorted(List(1, 1, 5, 100)))
    assert(List.isSorted(List(1, 1, 1, 1)))
    assert(List.isSorted(List(-3, -1 ,1, 2, 5, 100)))
  }

  test("list should not be sorted") {
    assert(!List.isSorted(List(1, 2, 5, 4, 100)))
  }
}
