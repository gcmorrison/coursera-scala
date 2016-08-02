object Lecture1_5 {
  // Sequential
  def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
    var i = s; var sum: Int = 0
    while (i < t) {
      sum = sum + power(a(i), p)
      i = i + 1
    }
    sum
  }

  def power(x: Int, p: Double): Int = Math.exp(p * Math.log(Math.abs(x))).toInt

  def pNormSeq(a: Array[Int], p: Double): Int = {
    power(sumSegment(a, p, 0, a.length), 1/p)
  }

  // Parallel combinator
  def pNormTwoPart(a: Array[Int], p: Double): Int = {
    val m = a.length / 2

    val (sum1, sum2) = parallel(sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))

    power(sum1 + sum2, 1/p)
  }

  // Recursive parallel segmentation
  val threshold = 2
  def pNormRec(a: Array[Int], p: Double): Int = power(segmentRec(a, p, 0, a.length), 1/p)

  def segmentRec(a: Array[Int], p: Double, s: Int, t: Int) = {
    if (t - s < threshold) sumSegment(a, p, s, t)
    else {
      val m = s + (t - s)/2
      val (sum1, sum2) = parallel(segmentRec(a, p, s, m), segmentRec(a, p, m, t))
      sum1 + sum2
    }
  }
}