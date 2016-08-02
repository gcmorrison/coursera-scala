object Lecture1_7 {

  trait Task[A] {
    def join: A
  }

  def task[A](c: => A): Task[A]

//  implicit def getJoin[T](x: Task[T]): T = x.join

  def parallel[A, B](cA: => A, cB: => B): (A, B) = {
    val tB: Task[B] = task { cB }
    val tA: A = cA
    (tA, tB.join)
  }
}