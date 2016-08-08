import scala.collection.mutable
import scala.collection.parallel.Task

object Lecture3_5 {
  // Iterator
  trait Iterator[T] {
    def hasNext: Boolean
    def next(): T
    def foldLeft[S](z: S)(f: (S, T) => S): S = {
      var result = z
      while (hasNext) result = f(result, next())
      result
    }
  }

  // Splitter
  val threshold = 10
  trait Splitter[T] {
    def split: Seq[Splitter[T]]
    def remaining: Int
    def fold[T](z: T)(f: (T, T) => T): T = {
      if (remaining < threshold) foldLeft(z)(f)
      else {
        val children: Seq[Task[T]] = for (child <- split) yield task {child.fold(z)(f)}
        children.map(_.join()).foldLeft(z)(f)
      }
    }
  }

  // Builder (Sequential)
  trait Traversable[T] {
    def foreach(f: T => Unit): Unit
    def newBuilder: mutable.Builder[T, Traversable[T]]
    def filter(p: T => Boolean): Traversable[T] = {
      val b = newBuilder
      foreach(x => if (p(x)) b += x)
      b.result
    }
  }

  // Combiner (Parallel version of Builder)
  trait Combiner[A, Repr] extends mutable.Builder[A, Repr] {
    def combine(that: Combiner[A, Repr]): Combiner[A, Repr]
    def newCombiner: Combiner[A, Repr]
  }
}