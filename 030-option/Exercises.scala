// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.option

// Exercise 1

trait OrderedPoint extends scala.math.Ordered[java.awt.Point]:

  this: java.awt.Point =>

  // I tried using this clean and logically correct solution, but it fails
  // tests due to integer overflow:
  //   scala> 2147483647 - (-1)
  //   val res2: Int = -2147483648
  //
  // "Clean solution": (this, that) match
  //     case (a, b) if a.x == b.x => a.y - b.y
  //     case (a, b) => a.x - b.x
  override def compare(that: java.awt.Point): Int =
    that match
      case t if x < t.x => -1
      case t if x > t.x => 1
      case t if y < t.y => -1
      case t if y > t.y => 1
      case _            => 0

// Try the following (and similar) tests in the repl (sbt console):
//
// import adpro._
// val p = new java.awt.Point(0, 1) with OrderedPoint
// val q = new java.awt.Point(0, 2) with OrderedPoint
// assert(p < q)

// Chapter 3 Exercises

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:

  // Exercise 2

  def size[A](t: Tree[A]): Int =
    t match
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size(l) + size(r)

  // Exercise 3

  def maximum(t: Tree[Int]): Int =
    t match
      case Leaf(v)      => v
      case Branch(l, r) => maximum(l) max maximum(r)

  // Exercise 4

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match
      case Leaf(v)      => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))

  // Exercise 5

  def fold[A, B](t: Tree[A])(f: (B, B) => B)(g: A => B): B =
    t match
      case Leaf(v)      => g(v)
      case Branch(l, r) => f(fold(l)(f)(g), fold(r)(f)(g))

  def size1[A](t: Tree[A]): Int =
    fold[A, Int](t)(1 + _ + _)(_ => 1)

  def maximum1(t: Tree[Int]): Int =
    fold[Int, Int](t)(_ max _)(identity)

  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(Branch(_, _))(v => Leaf(f(v)))

enum Option[+A]:
  case Some(get: A)
  case None

  // Exercise 6

  def map[B](f: A => B): Option[B] =
    this match
      case Some(v) => Some(f(v))
      case None    => None

  def getOrElse[B >: A](default: => B): B =
    this match
      case Some(v) => v
      case None    => default

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match
      case Some(v) => f(v)
      case None    => None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match
      case Some(_) => this
      case None    => ob

  def filter(p: A => Boolean): Option[A] =
    this match
      case Some(v) if p(v) => this
      case _               => None

  // Scroll down for Exercise 7, in the bottom of the file, outside Option

  def forAll(p: A => Boolean): Boolean = this match
    case None    => true
    case Some(a) => p(a)

object Option:

  // Exercise 9

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] =
    ???

  // Exercise 10

  def sequence[A](aos: List[Option[A]]): Option[List[A]] =
    ???

  // Exercise 11

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    ???

end Option

// Exercise that are outside the Option companion object

import Option.{Some, None}

def headOption[A](lst: List[A]): Option[A] = lst match
  case Nil    => None
  case h :: t => Some(h)

// Exercise 7

def headGrade(lst: List[(String, Int)]): Option[Int] =
  ???

def headGrade1(lst: List[(String, Int)]): Option[Int] =
  ???

// Implemented in the text book

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

// Exercise 8

def variance(xs: Seq[Double]): Option[Double] =
  ???

// Scroll up, to the Option object for Exercise 9
