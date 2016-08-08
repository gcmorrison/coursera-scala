object Lecture3_3 {
  // Fold parallelizable only if input and output types match
  def fold[A](z: A)(f: (A, A) => A): A = {
    ???
  }

  def sum(xs: Array[Int]): Int = {
    xs.par.fold(0)(_ + _)
  }

  def max(xs: Array[Int]): Int = {
    xs.par.fold(Int.MinValue)((x: Int, y: Int) => Math.max(x, y))
  }

  // Below example parallel execution is non-deterministic, i.e. unstable
  val rps = Array("paper", "rock", "paper", "scissors").par.fold("")(play)
  def play(a: String, b: String): String = List(a, b).sorted match {
    case List("paper", "scissors") => "scissors"
    case List("paper", "rock") => "paper"
    case List("rock", "scissors") => "rock"
    case List(a, b) if a == b => a
    case List("", b) => b
  }

  // Parallelizable implementation of fold that allows for arbitrary types
  def aggregate[A, B](z: B)(f: (B, A) => B, g: (B, B) => B): B = {
    ???
  }
}
