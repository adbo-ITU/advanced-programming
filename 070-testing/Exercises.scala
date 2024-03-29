// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary

import lazyList00.* // uncomment to test the book laziness solution implementation
// import lazyList01.* // uncomment to test the broken headOption implementation
// import lazyList02.* // uncomment to test another version

/* Generators and helper functions */

import LazyList.*

/** Convert a strict list to a lazy-list */
def list2lazyList[A](la: List[A]): LazyList[A] =
  LazyList(la*)

/** Generate finite non-empty lazy lists */
def genNonEmptyLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
  for la <- arbitrary[List[A]].suchThat { _.nonEmpty } yield list2lazyList(la)

def genLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
  for la <- arbitrary[List[A]] yield list2lazyList(la)

def genNonNegative[A](using Arbitrary[A]): Gen[Int] =
  arbitrary[Int].suchThat(_ != Int.MinValue).map(_.abs)

def genLazyListWithSize[A](using Arbitrary[A]): Gen[(Int, LazyList[A])] =
  for {
    n <- Gen.choose(0, 1000)
    la <- Gen.listOfN(n, arbitrary[A])
  } yield (n, list2lazyList(la))

/** Generate an infinite lazy list of A values.
  *
  * This lazy list is infinite if the implicit generator for A never fails. The
  * code is ugly-imperative, but it avoids stack overflow (as Gen.flatMap is not
  * tail recursive)
  */
def infiniteLazyList[A: Arbitrary]: Gen[LazyList[A]] =
  def loop: LazyList[A] =
    summon[Arbitrary[A]].arbitrary.sample match
      case Some(a) => cons(a, loop)
      case None    => empty
  Gen.const(loop)

/* The test suite */

object LazyListSpec extends org.scalacheck.Properties("testing"):

  // Exercise 1

  property("Ex01.01: headOption returns None on an empty LazyList") =
    empty.headOption == None

  property(
    "Ex01.02: headOption returns the head of the stream packaged in Some"
  ) =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int) =>
      cons(n, empty).headOption == Some(n)
    } :| "singleton" &&
    forAll { (s: LazyList[Int]) => s.headOption != None } :| "random"

  // Exercise 2

  property("Ex02.01: headOption does not force the tail of a lazy list") =
    forAll { (n: Int) =>
      cons(n, ???).headOption == Some(n)
    }

  // Exercise 3

  property(
    "Ex03.01: take does not force any heads nor any tails of the lazy list it manipulates"
  ) =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (s: LazyList[Int], n: Int) =>
      cons(???, cons(???, s)).take(n)
      true
    }

  // Exercise 4

  property("Ex04.01: take(n) does not force the (n+1)st head ever") =
    given Arbitrary[(Int, LazyList[Int])] = Arbitrary(genLazyListWithSize)
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (ns: (Int, LazyList[Int]), other: LazyList[Int]) =>
      val (n, s) = ns
      s.append(other.map(_ => ???)).take(n).toList == s.toList
    }

  // Exercise 5

  property(
    "Ex05.01: l.take(n).take(n) == l.take(n) for any lazy list s and any n"
  ) =
    given Arbitrary[LazyList[Int]] = Arbitrary(genLazyList[Int])
    given Arbitrary[Int] = Arbitrary(genNonNegative[Int])

    forAll { (l: LazyList[Int], n: Int) =>
      l.take(n).take(n).toList == l.take(n).toList
    }

  // Exercise 6

  property(
    "Ex06.01: l.drop(n).drop(m) == l.drop(n+m) for any n,m."
  ) =
    given Arbitrary[LazyList[Int]] = Arbitrary(genLazyList[Int])
    given Arbitrary[Int] = Arbitrary(genNonNegative[Int])

    forAll { (l: LazyList[Int], n: Int, m: Int) =>
      // We don't want to deal with integer overflow here, so make sure that
      // the sum doesn't overflow
      (n + m >= 0) ==> (l.drop(n).drop(m).toList == l.drop(n + m).toList)
    }

  // Exercise 7

  property(
    "Ex07.01: l.drop(n) does not force any of the dropped elements (heads)"
  ) =
    given Arbitrary[(Int, LazyList[Int])] = Arbitrary(genLazyListWithSize[Int])

    forAll { (nl1: (Int, LazyList[Int]), nl2: (Int, LazyList[Int])) =>
      val (n1, l1) = nl1
      val l2 = nl2._2

      def prependFailWhenForced[A](n: Int, acc: LazyList[A]): LazyList[A] =
        if n <= 0 then acc
        else prependFailWhenForced(n - 1, cons(???, acc))

      prependFailWhenForced(n1, l2).drop(n1).toList == l2.toList
    }

  // Exercise 8

  property(
    "Ex08.01: l.map(identity).toList == l.toList"
  ) =
    given Arbitrary[LazyList[Int]] = Arbitrary(genLazyList[Int])

    forAll { (l: LazyList[Int]) =>
      l.map(identity).toList == l.toList
    }

  // Exercise 9

  property(
    "Ex09.01: map terminates on infinite lazy lists"
  ) =
    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList[Int])

    forAll { (l: LazyList[Int]) =>
      l.map(identity)
      true
    }

  // Exercise 10

  property(
    "Ex10.01: a.append(b) adds all elements of b after a"
  ) =
    given Arbitrary[LazyList[Int]] = Arbitrary(genLazyList[Int])

    forAll { (a: LazyList[Int], b: LazyList[Int]) =>
      a.append(b).toList == (a.toList ::: b.toList)
    }

  property(
    "Ex10.02: a.append(b) does not force any elements of b"
  ) =
    given Arbitrary[LazyList[Int]] = Arbitrary(genLazyList[Int])

    forAll { (a: LazyList[Int], b: LazyList[Int]) =>
      a.append(b.map(_ => ???))

      // Would also expect the following to work, but foldRight is eager in our
      // LazyList implementation for some reason..
      //   a.map(_ => ???).append(b)

      true
    }
