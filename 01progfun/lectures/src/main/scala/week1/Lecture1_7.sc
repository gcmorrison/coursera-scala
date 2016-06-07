object Lecture1_7 {
  def factorial(n: Int): Int = {
    def accumulate(n: Int, acc: Int): Int = {
      if (n == 0) acc else accumulate(n - 1, n * acc)
    }

    accumulate(n, 1)
  }

  factorial(4)
}