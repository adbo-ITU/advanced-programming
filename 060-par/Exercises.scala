// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.par

import java.util.concurrent.{Executors, ExecutorService, Callable}

import scala.language.implicitConversions
import scala.io.Source

/* This non-blocking  version of Future (different from
 * java.util.concurrent) takes a continuation instead of returning a
 * value, and calls it when ready.
 */
opaque type Future[+A] = (A => Unit) => Unit

opaque type Par[A] = ExecutorService => Future[A]

/** This call, extracting the value will block the current thread on
  * latch.await. The users of API should not call it earlier than needed. Run
  * should be called as late as possible, and programming should proceed using
  * the other APIs to allow high concurrency (not wasting a thread).
  */
extension [A](pa: Par[A])

  def run(es: ExecutorService): A =
    val ref = java.util.concurrent.atomic.AtomicReference[A]()
    val latch = java.util.concurrent.CountDownLatch(1)
    pa(es) { a =>
      ref.set(a); latch.countDown
    }
    latch.await
    ref.get

  def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
    es =>
      k =>
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A, B]](es) {
          case Left(a) =>
            if br.isDefined then Par.eval(es)(k(f(a, br.get)))
            else ar = Some(a)
          case Right(b) =>
            if ar.isDefined then Par.eval(es)(k(f(ar.get, b)))
            else br = Some(b)
        }
        pa(es) { a => combiner ! Left(a) }
        pb(es) { b => combiner ! Right(b) }

  def map[B](f: A => B): Par[B] =
    pa.map2(Par.unit(())) { (a, _) => f(a) }

  // Scroll below to Exercise 1, in object Par
  // Come back here after solving Exercise 7

  // Exercise 8

  def chooser[B](f: A => Par[B]): Par[B] = ???

  // Exercise 9 continues in the Par object, in the bottom of the file

object Par:

  /** Pass the value to the continuation, the executor service is not used. Note
    * that unit is strict, so `a` will be computed in the current thread!
    */
  def unit[A](a: A): Par[A] =
    es => k => k(a)

  /** A non-strict version of `unit`, which may allow for parallelism. */
  def delay[A](a: => A): Par[A] =
    es => k => k(a)

  def fork[A](a: => Par[A]): Par[A] =
    es => k => eval(es)(a(es)(k))

  /** A helper function, for evaluating an action asynchronously, using the
    * given `ExecutorService`. This is just to avoid having to instantiate a new
    * Callable object every time we want to evaluate something in this file.
    * Taking an argument by-name is much idomatic to scala (and lazy
    * programming) than creating callable objects (Java-style). The
    * ExecutorService is a Java API so we create a small adaptation here to make
    * things nice.
    */
  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  // Exercise 1

  /* Write the answer here in a comment
   *
   * In the book's model of Par, if unit was strict, it would compute
   * the value of a in the current thread, not in parallel. Thus, the
   * strict unit would be a sequential computation.
   */

  // Exercise 2

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  // Exercise 3

  /* Write the answer here in a comment:
   *
   * I would ensure that the function properly composes the given Par
   * and does not execute the map function logic before being instructed
   * to run in parallel.
   *
   * Test case could be to have a side effect (such as print) within the
   * mapper function f, use map (but NOT run it), check that the side
   * effect has not been applied, then run the Par, and then check that
   * it has been applied.
   *
   * Alternatively, you could check that it is being run in parallel and
   * not blocking the current thread. For example, run an expensive task,
   * then make sure the main thread can keep going while map is running.
   */

  // Exercise 4

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(Nil))((pa, pz) =>
      pz.map2(pa)((z, a) => a :: z)
    )

  /* This is shown in the book: */
  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    sequence(as.map(asyncF(f)))

  // Exercise 5

  def wget(uris: String*): List[String] =
    val pool = java.util.concurrent.Executors.newFixedThreadPool(uris.size)

    val results = parMap(uris.toList)(uri =>
      scala.io.Source.fromURL(uri)("ISO-8859-1").mkString
    ).run(pool)

    pool.shutdown()
    results

  /* Write your explanation in English here:
   *
   * My solution obtains concurrency by using parMap. Inside parMap, each
   * item of the list is turned into an parallel task (via lazyUnit),
   * meaning the mappers (sending the requests) are running in parallel
   * and not blocking each other, assuming enouch threads are available
   * in the pool.
   *
   * Parallel runs in 320 ms most of the time. Non-parallel is 420-700ms. So
   * definitely a difference. Test:
   *   Par.wget("https://www.dr.dk", "https://www.itu.dk", "https://www.google.com")
   */

  // Exercise 6

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    as.foldRight[Par[List[A]]](unit(Nil))((a, pz) =>
      pz.map2(asyncF(f)(a))((z, r) => if r then a :: z else z)
    )

  // Exercise 7

  // Note: The solution from the text book does not apply immediately.
  // It has to be adapted to the non-blocking representation of Par and
  // Future that we are using in this chapter.
  def choiceN[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
    // I WANT this:
    //  pn.flatMap(n => choices(n))
    // But we have no flatMap.
    // This is incorrect, I think, since it runs all computations:
    pn.map2(sequence(choices))((n, c) => c(n))

  def choice[A](pb: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(pb.map(if _ then 0 else 1))(List(t, f))

  // Exercise 8 is found above, in the extension methods of Par[A]
  // Come back here when done with Exercise 8.

  // Exercise 9

  def join[A](p: Par[Par[A]]): Par[A] =
    ???

  def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    ???

end Par

// Exercise 10

// The types here are broken, and you need to fix them
// replace THIS with a meaningful name
extension (THIS: Any)
  def join: Any =
    ???

// The types here are broken, and you need to fix them
extension (THIS: Any)
  def choiceN(SOMETHING: Any): Any =
    ???

// The types here are broken, and you need to fix them
extension (THIS: Any)
  def choice(SOMETHING: Any, ANOTHERTHING: Any) =
    ???

/* Write your answer in English here:
 * ....
 */
