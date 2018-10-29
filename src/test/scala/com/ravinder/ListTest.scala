package com.ravinder

import com.ravinder.List.map
import com.ravinder.List.filter
import org.scalatest.{FunSuite, Matchers}

class ListTest extends FunSuite with Matchers {
  private def doubleIt(a: Int): Int = 2 * a

  private def isEven(num: Int): Boolean = num % 2 == 0

  private def isOdd(num: Int): Boolean = num % 2 != 0

  test("should create list from given numbers") {
    val list = List(1, 2)

    list shouldBe Cons(1, Cons(2, Nil))
  }

  test("should give nil when no argument is passed") {
    val list = List.empty()

    list shouldBe Nil
  }

  test("should pass all elements of given list to a given function") {
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
}
