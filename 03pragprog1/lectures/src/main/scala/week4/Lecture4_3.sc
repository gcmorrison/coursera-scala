import common._

object Lecture4_3 {

  sealed trait Tree[+T]

  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]

  case class Leaf[T](elem: T) extends Tree[T]

  case object Empty extends Tree[Nothing]

  def filter[T](t: Tree[T])(p: T => Boolean): Tree[T] = t match {
    case Node(left, right) => {
      val (l, r) = parallel(filter(left)(p), filter(right)(p))
      Node(l, r)
    }
    case Leaf(elem) => if (p(elem)) t else Empty
    case Empty => Empty
  }

  // Above Tree object is parallelizable, but not good since the result isn't necessarily parallelizable

  // Enter Conc
  sealed trait Conc[+T] {
    def level: Int

    def size: Int

    def left: Conc[T]

    def right: Conc[T]
  }

  case object Empty extends Conc[Nothing] {
    def level = 0

    def size = 0

    def left: Conc[Nothing] = this

    def right: Conc[Nothing] = this
  }

  class Single[T](val x: T) extends Conc[T] {
    def level = 0

    def size = 1

    def left: Conc[T] = Empty

    def right: Conc[T] = Empty
  }

  case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
    val level = 1 + math.max(left.level, right.level)
    val size = left.size + right.size

    def <>(that: Conc[T]): Conc[T] = {
      if (this == Empty) that
      else if (that == Empty) this
      else concat(this, that)
    }

    def concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
      val diff = ys.level - xs.level
      if (diff >= -1 && diff <= 1) new <>(xs, ys)
      else if (xs.left.level >= xs.right.level) {
        val nr = concat(xs.right, ys)
        new <>(xs.left, nr)
      } else {
        val nrr = concat(xs.right.right, ys)
        if (nrr.level == xs.level - 3) {
          val nl = xs.left
          val nr = new <>(xs.right.left, nrr)
          new <>(nl, nr)
        } else {
          val nl = new <>(xs.left, xs.right.left)
          val nr = nrr
          new <>(nl, nr)
        }
      }
    }
  }

}