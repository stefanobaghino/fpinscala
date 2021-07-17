package fpinscala.datastructures

sealed trait List[+A] { // `List` data type, parameterized on a type, `A`
  final override def toString(): String = List.toString(this)
}
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def toString[A](as: List[A]): String =
    as match {
      case Nil => "Nil"
      case Cons(h, t) =>
        val items = foldLeft(t, s"$h")((str, a) => s"$str, $a")
        s"List($items)"
    }

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    as.foldRight(Nil : List[A])(Cons(_, _))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h, Nil)
      case Cons(_, tail) => Cons(h, tail)
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(as: List[A], m: Int): List[A] =
      as match {
        case Cons(_, tail) if m > 0 => go(tail, m - 1)
        case _ => as
      }
    go(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(as: List[A]): List[A] =
      as match {
        case Cons(h, t) if f(h) => go(t)
        case _ => as
      }
    go(l)
  }

  // not stack-safe
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil | Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  // requires reversal
  // def init[A](l: List[A]): List[A] = {
  //   @annotation.tailrec
  //   def go(as: List[A], acc: List[A]): List[A] =
  //     as match {
  //       case Nil | Cons(_, Nil) => acc
  //       case Cons(h, t) => go(t, Cons(h, acc))
  //     }
  //   go(l, Nil)
  // }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, l) => l + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(as: List[A], acc: B): B = {
      as match {
        case Nil => acc
        case Cons(h, t) => go(t, f(acc, h))
      }
    }
    go(l, z)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil : List[A])((acc, a) => Cons(a, acc))

  def cat[A](ls: List[List[A]]): List[A] =
    ls match {
      case Nil => Nil
      case Cons(h, t) => foldLeft(t, h)(append)
    }

  def incremented(ns: List[Int]): List[Int] =
    map(ns)(_ + 1)

  def doublesToString(xs: List[Double]): List[String] =
    map(xs)(_.toString)

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil : List[B])((a, bs) => Cons(f(a), bs))

  def filter[A,B](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(a => if (p(a)) List(a) else Nil)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil : List[B])((a, bs) => append(f(a), bs))

}
