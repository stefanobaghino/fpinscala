package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList: List[A] =
    foldRight(List.empty[A])(_ :: _)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), countdown) if countdown > 0 => Some((h(), (t(), countdown - 1))) 
      case _ => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this)  {
      case Cons(h, t) =>
        lazy val head = h()
        if (p(head)) Some((head, t())) else None
      case _ =>
        None
    }

  def forAll(p: A => Boolean): Boolean =
    !exists(!p(_))

  def headOption: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, as) => if (p(a)) Stream.cons(a, as) else as)

  def append[AA >: A](that: Stream[AA]): Stream[AA] =
    foldRight(that)(Stream.cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, bs) => f(a).append(bs))

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, that)) {
      case (Cons(lh, lt), Cons(rh, rt)) => Some((f(lh(), rh()), (lt(), rt())))
      case _ => None
    }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Cons(lh, lt), Cons(rh, rt)) => Some(((Some(lh()), Some(rh())), (lt(), rt())))
      case (Empty, Cons(rh, rt)) => Some(((None, Some(rh())), (Empty, rt())))
      case (Cons(lh, lt), Empty) => Some(((Some(lh()), None), (lt(), Empty)))
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.nonEmpty).forAll { case (a, b) => a == b }

  def tails: Stream[Stream[A]] =
    cons(this, unfold(this) {
      case Empty =>
        None
      case Cons(_, t) =>
        lazy val tail = t()
        Some((tail, tail))
    })

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = constant(1)

  def constant(n: Int): Stream[Int] = unfold(n)(_ => Some(n, n))

  def from(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  lazy val fibs: Stream[Int] = unfold((0, 1)) { case (a, b) => Some((a, (b, b + a))) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Stream.empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

}