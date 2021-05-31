package com.ravinder

object HOF {
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b: B => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A => b: B => f(a, b)
  }

  def uncurry[A, B, C](curriedFn: A => B => C): (A, B) => C = {
    (a: A, b: B) => curriedFn(a)(b)
  }

  def compose[A, B, C](fn1: B => C, fn2: A => B): A => C = {
    a: A => fn1(fn2(a))
  }
}
