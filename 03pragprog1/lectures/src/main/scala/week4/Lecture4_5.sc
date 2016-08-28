import scala.annotation.tailrec
import scala.reflect.ClassTag

object Lecture4_5 {

  class ConcBuffer[T: ClassTag](val k: Int, private var conc: Conc[T]) {
    private var chunk: Array[T] = new Array(k)
    private var chunkSize: Int = 0

    final def +=(elem: T): Unit = {
      if (chunkSize >= k) expand()
      chunk(chunkSize) = elem
      chunkSize += 1
    }

    private def expand() {
      conc = appendLeaf(conc, new Chunk(chunk, chunkSize))
      chunk = new Array(k)
      chunkSize = 0
    }

    final def combine(that: ConcBuffer[T]): ConcBuffer[T] = {
      val combinedConc = this.result <> that.result
      new ConcBuffer(k, combinedConc)
    }

    def result: Conc[T] = {
      conc = appendLeaf(conc, new Chunk(chunk, chunkSize))
      conc
    }
  }

  class Chunk[T](val array: Array[T], val size: Int) extends Conc[T] {
    def level = 0
  }

  // Below copied from Lecture 4_3
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

  // Copied from Lecture 4_4
  case class Append[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
    val level = 1 + math.max(left.level, right.level)
    val size = left.size + right.size
  }

  def appendLeaf[T](xs: Conc[T], ys: Single[T]): Conc[T] = xs match {
    case Empty => ys
    case xs: Single[T] => new <>(xs, ys)
    case _ <> _ => new Append(xs, ys)
    case xs: Append[T] => append(xs, ys)
  }

  @tailrec private def append[T](xs: Append[T], ys: Conc[T]): Conc[T] = {
    if (xs.right.level > ys.level) new Append(xs, ys)
    else {
      val zs = new <>(xs.right, ys)
      xs.left match {
        case ws @ Append(_, _) => append(ws, zs)
        case ws if ws.level <= zs.level => ws <> zs
        case ws => new Append(ws, zs)
      }
    }
  }
}