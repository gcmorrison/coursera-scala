 import common._

object Lecture2_6 {
  // Sequential implementation of scanLeft
  def scanLeftSeq[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
    out(0) = a0
    var i = 0
    while (i < inp.length) {
      out(i + 1) = f(out(i), inp(i))
      i = i + 1
    }
  }

  val inp = List(1, 3, 8).toArray
  var out = new Array[Int](inp.length + 1)
  scanLeftSeq[Int](inp, 100, (x: Int, y) => x + y, out)
  out mkString " "

  // Parallel implementation of scanLeft
  def reduceSeg1[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A): A = {
    // Imagine this is implemented in parallel
  }
  def mapSeg[A, B](inp: Array[A], left: Int, right: Int, fi: (Int, A) => B, out: Array[B]): Unit = {
    // Imagine this is implemented in parallel
  }
  def scanLeftInParUsingParMapReduce[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]) = {
    val fi = { (i: Int, v: A) => reduceSeg1(inp, 0, i, a0, f)}
    mapSeg(inp, 0, inp.length, fi, out)
    val last = inp.length - 1
    out(last + 1) = f(out(last), inp(last))
  }

  // Tree example
  sealed abstract class Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  sealed abstract class TreeRes[A] {val res: A}
  case class LeafRes[A](override val res: A) extends TreeRes[A]
  case class NodeRes[A](l: TreeRes[A], override val res: A, r: TreeRes[A]) extends TreeRes[A]

  def reduceRes[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
    case Leaf(v) => LeafRes(v)
    case Node(l, r) => {
      val (tL, tR) = (reduceRes(l, f), reduceRes(r, f))
      NodeRes(tL, f(tL.res, tR.res), tR)
    }
  }

  def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
    case Leaf(v) => LeafRes(v)
    case Node(l, r) => {
      val (tL, tR) = parallel(upsweep(l, f), upsweep(r, f))
      NodeRes(tL, f(tL.res, tR.res), tR)
    }
  }

  def downsweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
    case LeafRes(v) => Leaf(f(a0, v))
    case NodeRes(l, _, r) => {
      val (tL, tR) = parallel(downsweep(l, a0, f), downsweep(r, f(a0, l.res), f))
      Node(tL, tR)
    }
  }

  // Implementation of scanLeft on trees
  def scanLeft[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
    val tRes = upsweep(t, f)
    val scan1 = downsweep(tRes, a0, f)
    prepend(a0, scan1)
  }

  def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
    case Leaf(v) => Node(Leaf(x), Leaf(v))
    case Node(l, r) => Node(prepend(x, l), r)
  }

  // Upsweep and Downsweep for arrays
  sealed abstract class TreeResA[A] { val res: A}
  case class LeafA[A](from: Int, to: Int, override val res: A) extends TreeResA[A]
  case class NodeA[A](l: TreeResA[A], override val res: A, r: TreeResA[A]) extends TreeResA[A]

  val threshold = 5
  def upsweep[A](inp: Array[A], from: Int, to: Int, f: (A, A) => A): TreeResA[A] = {
    if (to - from < threshold) {
      LeafA(from, to, reduceSeg1(inp, from + 1, to, inp(from), f))
    } else {
      val mid = from + (to - from) / 2
      val (tL, tR) = parallel(upsweep(inp, from, mid, f), upsweep(inp, mid, to, f))
      NodeA(tL, f(tL.res, tR.res), tR)
    }
  }
  def downsweep[A](inp: Array[A], a0: A, f: (A, A) => A, t: TreeResA[A], out: Array[A]): Unit = t match {
    case LeafA(from, to, res) => scanLeftSeg(inp, from, to, a0, f, out)
    case NodeA(l, _, r) => {
      val (_, _) = parallel(downsweep(inp, a0, f, l, out), downsweep(inp, f(a0, l.res), f, r, out))
    }
  }
  def scanLeftSeg[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A, out: Array[A]) = {
    if (left < right) {
      var i = left
      var a = a0
      while (i < right) {
        a = f(a, inp(i))
        i = i + 1
        out(i) = a
      }
    }
  }
  def scanLeft[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]) = {
    val t = upsweep(inp, 0, inp.length, f)
    downsweep(inp, a0, f, t, out)
    out(0) = a0
  }
}