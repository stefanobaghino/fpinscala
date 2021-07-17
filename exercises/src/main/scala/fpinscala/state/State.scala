package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  val nonNegativeInt: Rand[Int] = int.map(i => if (i < 0) -(i + 1) else i)

  val double: Rand[Double] =
    nonNegativeInt.map(_.toDouble / Int.MaxValue)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    ra.map2(rb)((_, _))

  val intDouble: Rand[(Int, Double)] =
    both(int, double)

  val doubleInt: Rand[(Double,Int)] =
    both(double, int)

  val double3: Rand[(Double, Double, Double)] =
    double.flatMap(x => double.flatMap(y => double.map(z => (x, y, z))))

  val ints: Int => Rand[List[Int]] =
    count => State.sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    nonNegativeInt.flatMap { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) {
        State.unit(mod)
      } else {
        nonNegativeLessThan(n)
      }
    }

}

case class State[S,+A](run: S => (A, S)) {
  import State._
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State {
      s =>
        val (a, next) = run(s)
        f(a).run(next)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
    State {
      s =>
        l.foldLeft((List.empty[A], s)) {
          case ((acc, s), state) =>
            val (a, next) = state.run(s)
            (a :: acc, next)
        }
    }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def step(input: Input)(machine: Machine): Machine =
    if (machine.candies > 0 && input == Coin && machine.locked) {
        machine.copy(locked = false, coins = machine.coins + 1)
    } else if (machine.candies > 0 && input == Turn && !machine.locked) {
        machine.copy(locked = true, candies = machine.candies - 1)
    } else {
      machine
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map(input => modify(step(input))))
      machine <- get
    } yield (machine.candies, machine.coins)

}
