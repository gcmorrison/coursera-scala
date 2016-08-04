import common._

object Lecture2_3 {
  val testList = List(1, 3, 8)

  testList.foldLeft(100)((s, x) => s - x)
  testList.foldRight(100)((s, x) => s - x)
  testList.reduceLeft((s, x) => s - x)
  testList.reduceRight((s, x) => s - x)

  // Representing expression tree
  sealed abstract class Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // Sequential tree reduce
  def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(v) => v
    case Node(left, right) => f(reduce(left, f), reduce(right, f))
  }

  //Example of tree reduce
  def tree = Node(Leaf(1), Node(Leaf(3), Leaf(8)))
  def fMinus = (x: Int, y: Int) => x - y
  def resSeq = reduce(tree, fMinus)
  println("Sequential Result = " + resSeq)

  // Parallel tree reduce
  def reducePar[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(v) => v
    case Node(left, right) => {
      val (leftValue, rightValue) = parallel(reducePar(left, f), reducePar(right, f))
      f(leftValue, rightValue)
    }
  }
  def resPar = reducePar(tree, fMinus)
  println("Parallel Result = " + resPar)

  // Tree toList
  def toList[A](t: Tree[A]): List[A] = t match {
    case Leaf(v) => List(v)
    case Node(left, right) => toList(left) ++ toList(right)
  }
  // Tree map
  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Node(left, right) => Node(map(left, f), map(right, f))
  }

  // toList in terms of map and reduce
  def toListAlt[A](t: Tree[A]): List[A] = {
//    reduce(map(t, x => List(x)), (x: List[A], y: List[A]) => x ++ y)
    reduce(map(t, List(_)), _ ++ _)
  }

  // reduceSeg on array to compute fold
  val threshold = 5
  def reduceSeg[A](inp: Array[A], left: Int, right: Int, f: (A, A) => A): A = {
    if (right - left < threshold) {
      var res = inp(left); var i = left + 1
      while (i < right) {
        res = f(res, inp(i))
        i = i + 1
      }
      res
    } else {
      val mid = left + (right - left) / 2
      val (a1, a2) = parallel(reduceSeg(inp, left, mid, f), reduceSeg(inp, mid, right, f))
      f(a1, a2)
    }
  }
  def reduce[A](inp: Array[A], f: (A, A) => A): A = reduceSeg(inp, 0, inp.length, f)
  
}