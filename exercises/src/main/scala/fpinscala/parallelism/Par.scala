package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }
  
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(fork(map2(pa, pb)((a, b) => f(a, b, _))), fork(pc))((g, c) => g(c))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(lazyUnit(List.empty[A]))(map2(_, _)(_ :: _))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] =
    fork {
      sequence(ps.map(asyncF(f)))
    }

  def parFilter[A](ps: List[A])(p: A => Boolean): Par[List[A]] =
    map(parMap(ps)(a => (a, p(a))))(_.collect { case (a, keep) if keep => a })

  def parFold[A, B](ps: IndexedSeq[A], z: B)(m: A => B)(f: (B, B) => B): Par[B] =
    if (ps.size <= 1) {
      unit(ps.headOption.fold(z)(m))
    } else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(fork(parFold(l, z)(m)(f)), fork(parFold(r, z)(m)(f)))(f)
    }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => choices(run(es)(n).get)(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(if (_) 0 else 1))(List(t, f))

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {

  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    parFold(ints, 0)(identity)(_ + _)

  def lift1[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  def lift2[A, B, C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] = (a, b) => a.flatMap(aa => b.map(f(aa, _)))

  def none[A]: Option[A] = None

  def max(ints: IndexedSeq[Int]): Par[Option[Int]] =
    parFold(ints, none[Int])(Option(_))(lift2(math.max))

  def wc(paragraphs: IndexedSeq[String]): Par[Int] =
    parFold(paragraphs, 0)(_.length)(_ + _)

}
