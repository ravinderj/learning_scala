package com.ravinder

sealed trait List[+A] {

  def head: A

  def tail: List[A]

  def map[B](op: A => B): List[B]

  def filter(op: A => Boolean): List[A]

  def reduce[B](op: (A, B) => B, start: B): B

  def reverse: List[A]
}

case object Nil extends List[Nothing] {

  override def head: Nothing = throw new Exception("can't get head of a nil list")

  override def tail: List[Nothing] = throw new Exception("can't get tail of a nil list")

  override def map[B](op: Nothing => B): List[B] = this

  override def filter(op: Nothing => Boolean): List[Nothing] = this

  override def reduce[B](op: (Nothing, B) => B, start: B): B = start

  override def reverse: List[Nothing] = this
}

case class Cons[A](h: A, t: List[A]) extends List[A] {

  override def head: A = h

  override def tail: List[A] = t

  override def map[B](op: A => B): List[B] = {
    List(this) match {
      case Nil => Nil
      case _ => Cons(op(h), t.map(op))
    }
  }

  override def filter(op: A => Boolean): List[A] = {
    List(this) match {
      case Nil => Nil
      case _ => if (op(h)) Cons(h, t.filter(op)) else t.filter(op)
    }
  }

  override def reduce[B](op: (A, B) => B, start: B): B = {
    List(this) match {
      case Nil => start
      case _ => op(h, t.reduce(op, start))
    }
  }

  override def reverse(): List[A] = {
    def go(initial: List[A], list: List[A]): List[A] = {
      list match {
        case Nil => initial
        case Cons(h, t) => go(Cons(h, initial), t)
      }
    }

    go(Nil, this)
  }
}

object List {

  def apply[A](a: A*): List[A] = {

    def go(seq: Seq[A]): List[A] = {
      seq match {
        case Seq() => Nil
        case _ => Cons(seq.head, go(seq.tail))
      }
    }

    go(a.toSeq)
  }

  def empty() = Nil

  def isEmpty[A](list: List[A]): Boolean = {
    list match {
      case Nil => true
      case _ => false
    }
  }
}
