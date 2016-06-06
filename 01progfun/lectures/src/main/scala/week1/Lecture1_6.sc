object Lecture1_6 {
  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    def isGoodEnough(guess: Double): Boolean = {
      Math.abs(guess * guess - x) / x < 0.001
    }

    def improve(guess: Double): Double = {
      (guess + x / guess) / 2
    }

    sqrtIter(1)
  }

  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)
  sqrt(0.001)
  sqrt(0.1e-20)
  sqrt(1.0e20)
  sqrt(1.0e50)
}