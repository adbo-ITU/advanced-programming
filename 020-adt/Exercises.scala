// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.adt

import java.util.NoSuchElementException

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:

  def head[A](l: List[A]): A = l match
    case Nil        => throw NoSuchElementException()
    case Cons(h, _) => h

  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def append[A](l1: List[A], l2: List[A]): List[A] =
    l1 match
      case Nil        => l2
      case Cons(h, t) => Cons(h, append(t, l2))

  def foldRight[A, B](l: List[A], z: B, f: (A, B) => B): B = l match
    case Nil         => z
    case Cons(a, as) => f(a, foldRight(as, z, f))

  def map[A, B](l: List[A], f: A => B): List[B] =
    foldRight[A, List[B]](l, Nil, (a, z) => Cons(f(a), z))

  // Exercise 1 (is to be solved without programming)

  // Exercise 2

  def tail[A](l: List[A]): List[A] =
    l match
      case Nil        => throw NoSuchElementException()
      case Cons(_, t) => t

  // Exercise 3

  def drop[A](l: List[A], n: Int): List[A] =
    l match
      case _ if n <= 0 => l
      case Cons(_, t)  => drop(t, n - 1)
      case Nil         => throw NoSuchElementException()

  // Exercise 4

  def dropWhile[A](l: List[A], p: A => Boolean): List[A] =
    l match
      case Cons(h, t) if p(h) => dropWhile(t, p)
      case _                  => l

  // Exercise 5

  // Takes O(n) time since the whole list is traversed.
  // Takes O(n) space too since the root of the list is "changed".
  def init[A](l: List[A]): List[A] =
    l match
      case Cons(_, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))
      case Nil          => throw NoSuchElementException()

  // Exercise 6

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, len) => len + 1)

  // Exercise 7

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B, f: (B, A) => B): B =
    l match
      case Cons(h, t) => foldLeft(t, f(z, h), f)
      case Nil        => z

  // Exercise 8

  def product(as: List[Int]): Int =
    foldLeft(as, 1, _ * _)

  def length1[A](as: List[A]): Int =
    foldLeft(as, 0, (z, _) => 1 + z)

  // Exercise 9

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A], (z, h) => Cons(h, z))

  // Exercise 10

  def foldRight1[A, B](l: List[A], z: B, f: (A, B) => B): B = ???

  // Exercise 11

  def foldLeft1[A, B](l: List[A], z: B, f: (B, A) => B): B = ???

  // Exercise 12

  def concat[A](l: List[List[A]]): List[A] = ???

  // Exercise 13

  def filter[A](l: List[A], p: A => Boolean): List[A] = ???

  // Exercise 14

  def flatMap[A, B](l: List[A], f: A => List[B]): List[B] = ???

  // Exercise 15

  def filter1[A](l: List[A], p: A => Boolean): List[A] = ???

  // Exercise 16

  def addPairwise(l: List[Int], r: List[Int]): List[Int] = ???

  // Exercise 17

  def zipWith[A, B, C](l: List[A], r: List[B], f: (A, B) => C): List[C] = ???

  // Exercise 18

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
