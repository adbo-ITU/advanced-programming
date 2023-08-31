// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

// Solved in collaboration with Joachim Borup <aljb@itu.dk>

package adpro.intro

object MyModule:

  def abs(n: Int): Int =
    if n < 0 then -n else n

  // Exercise 1

  def square(n: Int): Int =
    n * n

  private def formatAbs(x: Int): String =
    s"The absolute value of ${x} is ${abs(x)}"

  val magic: Int = 42
  var result: Option[Int] = None

  @main def printAbs: Unit =
    assert(magic - 84 == magic.-(84))
    println(formatAbs(magic - 100))
    println(s"And the square is ${square(magic)}")

end MyModule

// Exercise 2 requires no programming

// Exercise 3

def fib(n: Int): Int =
  @annotation.tailrec
  def f(n: Int, f1: Int, f2: Int): Int =
    if n <= 0 then f1
    else f(n - 1, f2, f1 + f2)

  f(n - 1, 0, 1)

// Exercise 4

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
  @annotation.tailrec
  def checker(i: Int, l: Array[A]): Boolean =
    if i >= l.length - 1 then true
    else if !ordered(l(i), l(i + 1)) then false
    else checker(i + 1, l)

  checker(0, as)

// Exercise 5

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a) => f(a, _)

def isSortedCurried[A]: Array[A] => ((A, A) => Boolean) => Boolean =
  curry(isSorted)

// Exercise 6

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  ???

def isSortedCurriedUncurried[A]: (Array[A], (A, A) => Boolean) => Boolean =
  ???

// Exercise 7

def compose[A, B, C](f: B => C, g: A => B): A => C =
  ???
