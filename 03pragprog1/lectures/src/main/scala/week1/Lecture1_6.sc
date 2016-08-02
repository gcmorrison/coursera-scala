import scala.util.Random

object Lecture1_6 {
  def mcCount(iter: Int): Int = {
    val randomX = new Random
    val randomY = new Random
    var hits = 0
    for (i <- 0 until iter) {
      val x = randomX.nextDouble()
      val y = randomY.nextDouble()
      if (x * x + y * y < 1) hits = hits + 1
    }
    hits
  }

  def monteCarloPiSeq(iter: Int): Double = 4.0 * mcCount(iter) / iter

  monteCarloPiSeq(10000000)
}