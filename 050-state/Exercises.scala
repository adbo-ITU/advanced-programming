// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.state

import adpro.lazyList.LazyList
import adpro.lazyList.LazyList.*

trait RNG:
  /** Generate a random `Int`. We define other functions using `nextInt`. */
  def nextInt: (Int, RNG)

object RNG:

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      // The next state, which is an `RNG` instance created from the new seed.
      val nextRNG = SimpleRNG(newSeed)
      // `>>>` is right binary shift with zero fill.
      // The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG)

  // Exercise 1

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, nextRng) = rng.nextInt
    if n != Int.MinValue then (n.abs, nextRng) else nonNegativeInt(nextRng)

  // Exercise 2

  def double(rng: RNG): (Double, RNG) =
    val (n, nextRng) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, nextRng)

  // Exercise 3

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, rng1) = nonNegativeInt(rng)
    val (d, rng2) = double(rng1)
    ((i, d), rng2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), nextRng) = intDouble(rng)
    ((d, i), nextRng)

  // Exercise 4

  def ints(size: Int)(rng: RNG): (List[Int], RNG) =
    size match
      case s if s <= 0 => (List.empty, rng)
      case s =>
        val (l, rng1) = ints(s - 1)(rng)
        val (i, rng2) = int(rng1)
        (i :: l, rng2)

  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

  // Exercise 5

  lazy val double2: Rand[Double] =
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  // Exercise 6

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    ???

  // Exercise 7

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    ???

  def ints2(size: Int): Rand[List[Int]] =
    ???

  // Exercise 8

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    ???

  def nonNegativeLessThan(bound: Int): Rand[Int] =
    ???

end RNG

import State.*

case class State[S, +A](run: S => (A, S)):

  // Exercise 9 (methods in class State)
  // Search for the second part (sequence) below

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???

  def map[B](f: A => B): State[S, B] =
    ???

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???

object State:

  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def modify[S](f: S => S): State[S, Unit] = for
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Now Rand can be redefined like this (we keep it here in the State object,
  // to avoid conflict with the other Rand in RNG).
  type Rand[A] = State[RNG, A]

  // Exercise 9 (sequence, continued)

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    ???

  import adpro.lazyList.LazyList

  // Exercise 10 (stateToLazyList)

  def stateToLazyList[S, A](s: State[S, A])(initial: S): LazyList[A] =
    ???

  // Exercise 11 (lazyInts out of stateToLazyList)

  def lazyInts(rng: RNG): LazyList[Int] =
    ???

  lazy val tenStrictInts: List[Int] =
    ???

end State
