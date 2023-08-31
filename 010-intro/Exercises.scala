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
  ???

// Exercise 5

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  ???

def isSortedCurried[A]: Array[A] => ((A, A) => Boolean) => Boolean =
  ???

// Exercise 6

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  ???

def isSortedCurriedUncurried[A]: (Array[A], (A, A) => Boolean) => Boolean =
  ???

// Exercise 7

def compose[A, B, C](f: B => C, g: A => B): A => C =
  ???
