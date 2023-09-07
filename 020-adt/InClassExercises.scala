object InClassExercises {
  def f(n: Int): Int = {
    println(2 * n)
    if (n <= 0) 0 else f(n - 1)
    n
  }

  def main(args: Array[String]) = {
    println(s"Original input was ${f(12)}")
  }
}
