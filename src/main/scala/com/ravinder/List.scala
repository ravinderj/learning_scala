package com.ravinder

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[A](h: A, t: List[A]) extends List[A]

object List {
  def empty() = Nil

  def apply[A](a: A*): List[A] = {

    def go(seq: Seq[A]): List[A] = {
      seq match {
        case Seq() => Nil
        case _ => Cons(seq.head, go(seq.tail))
      }
    }

    go(a.toSeq)
  }

  def head[A](list: List[A]): A = {
    list match {
      case Cons(h, _) => h
      case Nil => throw new Exception("can't get head of a nil list")
    }
  }

  def tail[A](list: List[A]): List[A] = {
    list match {
      case Cons(_, t) => t
      case Nil => throw new Exception("can't get tail of a nil list")
    }
  }

  def isEmpty[A](list: List[A]): Boolean = {
    list match {
      case Nil => true
      case _ => false
    }
  }

  def map[A, B](list: List[A], op: A => B): List[B] = {
    list match {
      case Nil => Nil
      case _ => Cons(op(List.head(list)), List.map[A, B](List.tail(list), op))
    }
  }

  def filter[A](list: List[A], op: A => Boolean): List[A] = {
    list match {
      case Nil => Nil
      case Cons(h, t) => if (op(h)) Cons(h, filter(t, op)) else filter(t, op)
    }
  }
}
