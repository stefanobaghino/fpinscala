package fpinscala.datastructures

import annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r)
    }

  def max(t: Tree[Int]): Int =
    t match {
      case Leaf(n) => n
      case Branch(l, r) => max(l).max(max(r))
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => (depth(l) + 1).max(depth(r) + 1)
    }

}