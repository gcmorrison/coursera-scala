import scala.collection.{GenSet, mutable}

object Lecture3_4 {
  // Rule 1: Never write to a collection that is concurrently traversed
  def intersection(a: GenSet[Int], b: GenSet[Int]): mutable.Set[Int] = {
    val result = mutable.Set[Int]()
    for (x <- a) if (b contains x) result += x
    result
  }
  val seq = intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
  val par = intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)
  println(s"seq.size ${seq.size} != par.size ${par.size}")

  // Rule 2: Never read from a collection that is concurrently modified
  val graph = mutable.Map[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
  for ((k, v) <- graph.par) graph(k) = if (k == graph.size - 1) 0 else graph(v)
  val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })
  // Violation should be None
  println(s"violation: $violation")
}