object Lecture5_5 {
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())(f(_) :: _)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((x, y) => y + 1)

  val nums: List[Int] = List(2, -4, 5, 7, 1)
  mapFun(nums, (x: Int) => x * 2)
  lengthFun(nums)
}