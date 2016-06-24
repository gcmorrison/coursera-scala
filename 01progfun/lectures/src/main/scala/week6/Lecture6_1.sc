object Lecture6_1 {
  val M = 5
  val N = 3
  (1 to M) flatMap (x => (1 to N) map (y => (x, y)))

  def scalarProduct1(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(xy => xy._1 * xy._2).sum

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map{ case (x, y) => x * y }.sum

  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
}