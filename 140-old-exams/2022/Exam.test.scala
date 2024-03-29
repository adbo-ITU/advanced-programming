/* This file is empty on purpose.   It is added, and configured if you
 * wanted to add your own tests during the exam.  It is not graded and
 * should not be submitted.
 */
package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}

object Exam2022AutumnSpec extends org.scalacheck.Properties("exam-2022"):

  // Q1

  property("A test that always passes (a sanity check)") = forAll { (n: Int) =>
    n == n
  }

  property("AQ1.1") = forAll { (n: Int) =>
    val f = (x: Int) => x + n
    ExceptionalOptions.SafeTotal(f)(n).isDefined
  }

  property("AQ1.2") = forAll { (n: Int) =>
    val f = (x: Int) => ???
    ExceptionalOptions.SafeTotal(f)(n).isEmpty
  }

  property("AQ2.1") = forAll { (l: List[Int]) =>
    ExceptionalOptions.headOption(l) == l.headOption
  }
