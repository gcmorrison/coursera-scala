object Lecture4_1 {
  object List {
    def apply() = new Nil

    def apply(i: Int) = new Cons(i, new Nil)

    def apply(i: Int, j: Int) = new Cons(i, new Cons(j, new Nil))
  }

  println(List(0))
}

Lecture4_1

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true

  def tail: List[T] = throw new NoSuchElementException("Nil.tail called")

  def head: T = throw new NoSuchElementException("Nil.head called")
}