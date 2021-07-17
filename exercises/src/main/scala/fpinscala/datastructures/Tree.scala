package fpinscala.datastructures

import annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _)

  def max(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  def depth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => (l + 1).max(r + 1))

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)) : Tree[B])(Branch(_, _))

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B =
    t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

}