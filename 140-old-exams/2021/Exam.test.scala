/** This file is empty on purpose. It is added, and configured if you wanted to
  * experiment with tests.
  */

package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}

import fpinscala.answers.state.RNG

object Exam2021AutumnSpec extends org.scalacheck.Properties("exam-2021"):

  // Q1

  property("Q1.1: A test that always passes (a sanity check)") = forAll {
    (n: Int) => n == n
  }

  property("Q2.1") = forAll { () =>
    Q2.sequence(List(Left("ahh"), Right(123))) == Left("ahh")
  }

  property("Q2.2") = forAll { () =>
    Q2.sequence(List(Left("ahh"), Right(123), Left("bahh"))) == Left("bahh")
  }

  property("Q2.3") = forAll { () =>
    Q2.sequence(List(Right(123), Right(456))) == Right(List(123, 456))
  }

  property("Q2.4") = forAll { () =>
    Q2.sequence(List()) == Right(List())
  }

  property("Q4.1") = forAll { (seed: Long) =>
    val (l, r, x) = Q4.riid.run(RNG.Simple(seed))._1
    l <= r && l.toDouble <= x && x <= r.toDouble
  }
