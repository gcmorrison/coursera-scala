object Lecture2_2 {
  // Example creations
  val xs = Stream.cons(1, Stream.cons(2, Stream.empty))
  val xs2 = (1 to 1000).toStream
  val lr = listRange(1, 10)
  val sr = streamRange(1, 10)
  val csr = consStreamRange(1, 10)
  val esr = exerciseStreamRange(1, 10).take(3).toList

  // Comparing Stream to List
  def streamRange(lo: Int, hi: Int): Stream[Int] =
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))

  def listRange(lo: Int, hi: Int): List[Int] =
    if (lo >= hi) Nil
    else lo :: listRange(lo + 1, hi)

  // Building Stream like List
  def consStreamRange(lo: Int, hi: Int): Stream[Int] =
    if (lo >= hi) Stream.empty
    else lo #:: streamRange(lo + 1, hi)

  // Exercise Stream
  def exerciseStreamRange(lo: Int, hi: Int): Stream[Int] = {
    print(lo + " ")
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))
  }
}