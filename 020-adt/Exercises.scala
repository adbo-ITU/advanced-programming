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

  def foldRight1[A, B](l: List[A], z: B, f: (A, B) => B): B =
    foldLeft(reverse(l), z, (a, b) => f(b, a))

  // Exercise 11

  def foldLeft1[A, B](l: List[A], z: B, f: (B, A) => B): B =
    foldRight[A, B => B](l, identity, (a, c) => b => c(f(b, a)))(z)

  // Exercise 12

  // Takes linear time in total length of all lists because append is
  // linear in its left argument (based on its source code). And each
  // sublist is given as the left argument.
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A], append(_, _))

  // Exercise 13

  def filter[A](l: List[A], p: A => Boolean): List[A] =
    foldRight(l, Nil: List[A], (a, z) => if p(a) then Cons(a, z) else z)

  // Exercise 14

  def flatMap[A, B](l: List[A], f: A => List[B]): List[B] =
    concat(map(l, f))
    // Scala's type inference can't figure this out: (concat compose map)(l, f)

  // Exercise 15

  def filter1[A](l: List[A], p: A => Boolean): List[A] =
    flatMap(l, a => if p(a) then List(a) else Nil)

  // Exercise 16

  // I know they said no standard recursion, but there is no zip or similar for our List
  def addPairwise(l: List[Int], r: List[Int]): List[Int] =
    (l, r) match
      case (Cons(lh, lt), Cons(rh, rt)) => Cons(lh + rh, addPairwise(lt, rt))
      case (Nil, _) | (_, Nil)          => Nil

  // Exercise 17

  def zipWith[A, B, C](l: List[A], r: List[B], f: (A, B) => C): List[C] =
    (l, r) match
      case (Cons(lh, lt), Cons(rh, rt)) => Cons(f(lh, rh), zipWith(lt, rt, f))
      case (Nil, _) | (_, Nil)          => Nil

  // Exercise 18

  // Very much inspired by a discussion with the legendary Joachim <aljb>
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match
      case (_, Nil)                                 => true
      case (Nil, _)                                 => false
      case (Cons(ah, at), Cons(bh, bt)) if ah == bh => hasSubsequence(at, bt)
      case (Cons(_, at), b)                         => hasSubsequence(at, b)
