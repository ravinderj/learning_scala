package com.ravinder

import org.scalatest.{FunSuite, Matchers}

class ListTest extends FunSuite with Matchers {
  private def doubleIt(a: Int): Int = 2 * a

  private def isEven(num: Int): Boolean = num % 2 == 0

  private def isOdd(num: Int): Boolean = num % 2 != 0

  private def sum(a: Int, b: Int): Int = a + b

  private def multiply(a: Int, b: Int): Int = a * b

  private def ascending(a: Int, b: Int): Boolean = a <= b

  private def strSizeAscending(a: String, b: String): Boolean = a.length <= b.length

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

  test("number list should be sorted") {
    assert(List.isSorted(List(), ascending))
    assert(List.isSorted(List(1), ascending))
    assert(List.isSorted(List(1, 2), ascending))
    assert(List.isSorted(List(1, 2, 5, 100), ascending))
    assert(List.isSorted(List(1, 1, 5, 100), ascending))
    assert(List.isSorted(List(1, 1, 1, 1), ascending))
    assert(List.isSorted(List(-3, -1 ,1, 2, 5, 100), ascending))
  }

  test("string list should be sorted by size") {
    assert(List.isSorted(List(), strSizeAscending))
    assert(List.isSorted(List("earth"), strSizeAscending))
    assert(List.isSorted(List("earth", "jupiter"), strSizeAscending))
    assert(List.isSorted(List("mars", "earth", "uranus", "jupiter"), strSizeAscending))
    assert(List.isSorted(List("mars", "mars", "uranus", "jupiter"), strSizeAscending))
    assert(List.isSorted(List("earth", "venus", "earth", "venus"), strSizeAscending))
  }

  test("number list should not be sorted") {
    assert(!List.isSorted(List(2, 1), ascending))
    assert(!List.isSorted(List(1, 2, 5, 4, 100), ascending))
  }

  test("string list should not be sorted by size") {
    assert(!List.isSorted(List("jupiter", "earth"), strSizeAscending))
    assert(!List.isSorted(List("mars", "earth", "uranus", "venus", "jupiter"), strSizeAscending))
  }
}
