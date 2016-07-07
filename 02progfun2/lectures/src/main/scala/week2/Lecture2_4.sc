object Lecture2_4 {
  val allNaturalNumbers = from(0)
  val allMultsOf4 = allNaturalNumbers map (_ * 4)

  // Infinite stream.  NOTE:  Doesn't crash program
  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  (allMultsOf4 take 100).toList

  // Calculate primes using Sieve's technique and infinite streams
  def sieve(s: Stream[Int]): Stream[Int] = s.head #:: sieve(s.tail filter (_ % s.head != 0))

  sieve(from(2)) take 100 toList

  // Calculate Sqrt using infinite streams
  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }

  sqrtStream(9) take 10 toList

  def isGoodEnough(guess: Double, x: Double) = math.abs((guess * guess - x) / x) < 0.0001

  sqrtStream(9).filter(isGoodEnough(_, 9)).take(5).toList
}