trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend [U >: T] (elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true

  def tail = throw new NoSuchElementException("Nil.tail")

  def head = throw new NoSuchElementException("Nil.head")
}

object Lecture4_4 {
  val x: List[String] = Nil
}