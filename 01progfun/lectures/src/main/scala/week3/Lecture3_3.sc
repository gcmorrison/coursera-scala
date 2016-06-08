object Lecture3_3 {
  def nth[T](n: Int, list: List[T]): T = {
    if (list.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) list.head
    else nth(n - 1, list.tail)
  }

  println(nth(2, new Cons(1, new Cons(2, new Cons(3, new Nil)))))
}

Lecture3_3

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