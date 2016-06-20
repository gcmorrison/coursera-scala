object Lecture5_2 {
  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def mergeLong(xs: List[Int], ys: List[Int]): List[Int] = xs match {
        case Nil => ys
        case x :: xs1 => ys match {
          case Nil => xs
          case y :: ys1 =>
            if (x < y) x :: mergeLong(xs1, ys)
            else y :: mergeLong(xs, ys1)
        }
      }
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  msort(List(1, 9, 7, 5, 3, 4, 6, 8))
}