object Lecture3_1 {
  def initializeArray(xs: Array[Int])(v: Int): Unit = {
    for (i <- (0 until xs.length).par) {
      xs(i) = v
    }
  }
  val arr = new Array[Int](10)
  initializeArray(arr)(5)
  arr mkString " "

  // Mandelbrot Set Example in lecture
}