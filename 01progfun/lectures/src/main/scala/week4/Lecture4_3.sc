object Lecture4_3 {
  val a: Array[NonEmpty] = Array(new NonEmpty(1, new Empty, new Empty))
  val b: Array[IntSet] = a
  b(0) = new Empty
  val s: NonEmpty = a(0)
}

abstract class IntSet {
}

class Empty extends IntSet {
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
}