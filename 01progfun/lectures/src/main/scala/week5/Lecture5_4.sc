object Lecture5_4 {
  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => (y * y) :: squareList(ys)
  }

  val tests = List(1, 2, 3, 4, 5)
  assert(squareList(tests) == (tests map (x => x * x)))

  val fruit = List("apple", "pineapple", "pear", "orange", "banana")

  val nums = List(2, -4, 5, 7, 1)
  nums filter (x => x > 0)
  nums filterNot (x => x > 0)
  nums partition (x => x > 0)

  nums takeWhile (x => x > 0)
  nums dropWhile (x => x > 0)
  nums span (x => x > 0)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val pair = xs span (y => y == x)
      pair._1 :: pack(pair._2)
  }
  pack(List("a", "a", "a", "b", "c", "c", "a"))

  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs) map (ys => (ys.head, ys.length))
  }
  encode(List("a", "a", "a", "b", "c", "c", "a"))
}