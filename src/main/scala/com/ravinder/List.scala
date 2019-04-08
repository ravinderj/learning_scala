package com.ravinder

sealed trait List[+A] {

  def head: A

  def tail: List[A]

  def map[B](op: A => B): List[B]

  def filter(op: A => Boolean): List[A]

  def reduce[B](start: B)(op: (A, B) => B): B

  def reverse: List[A]

  def +[B >: A](l: List[B]): List[B] = {
    this.reduce(l)((ele, acc) => Cons(ele, acc))
  }

  def flatMap[B](op: A => List[B]): List[B]
}

case object Nil extends List[Nothing] {

  override def head: Nothing = throw new Exception("can't get head of a nil list")

  override def tail: List[Nothing] = throw new Exception("can't get tail of a nil list")

  override def map[B](op: Nothing => B): List[B] = this

  override def filter(op: Nothing => Boolean): List[Nothing] = this

  override def reduce[B](start: B)(op: (Nothing, B) => B): B = start

  override def reverse: List[Nothing] = this

  override def flatMap[B](op: Nothing => List[B]): List[B] = this
}

case class Cons[A](h: A, t: List[A]) extends List[A] {

  override def head: A = h

  override def tail: List[A] = t

  override def map[B](op: A => B): List[B] = {
    this match {
      case Seq() => Nil
      case _ => Cons(op(h), t.map(op))
    }
  }

  override def filter(op: A => Boolean): List[A] = {
    this match {
      case Seq() => Nil
      case _ => if (op(h)) Cons(h, t.filter(op)) else t.filter(op)
    }
  }

  override def reduce[B](start: B)(op: (A, B) => B): B = {
    this match {
      case Seq() => start
      case _ => op(h, t.reduce(start)(op))
    }
  }

  override def reverse: List[A] = {
    def go(initial: List[A], list: List[A]): List[A] = {
      list match {
        case Nil => initial
        case Cons(h, t) => go(Cons(h, initial), t)
      }
    }

    go(Nil, this)
  }

  override def flatMap[B](op: A => List[B]): List[B] = {
    reduce(List[B]())((ele, acc) => op(ele) + acc)
  }
}

object List {

  def apply[A](a: A*): List[A] = {
    a.toSeq match {
      case Seq() => Nil
      case l => Cons(l.head, apply(l.tail: _*))
    }
  }

  def empty: Nil.type = Nil

  def isEmpty[A](list: List[A]): Boolean = {
    list == Nil
  }
}
