object Lecture3_2 {
  // foldLeft doesn't allow parallel execution, even though the collection is said to run in parallel
  def sum(xs: Array[Int]): Int = xs.par.foldLeft(0)(_ + _)
  sum(Array(1, 2, 3, 4, 5))

  // Implementing fold with the below signature allows parallelism, because the input and output types are the same
  def fold[A](z: A)(f: (A, A) => A): A = {

  }
}