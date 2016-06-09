object Lecture4_2 {
  println(Zero)
}

Lecture4_2

abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true

  def predecessor: Nat = throw new Exception("0.predecessor")

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = if (that.isZero) this else throw new Exception("0 - NonZero")
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false

  def predecessor: Nat = n

  def +(that: Nat): Nat = new Succ(n + that)

  def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}

//// E.G:
//abstract class Boolean {
//  def ifThenElse[T](t: => T, e: => T): T
//
//  def && (x: => Boolean): Boolean = ifThenElse(x, false)
//  def || (x: => Boolean): Boolean = ifThenElse(true, x)
//  def unary_! : Boolean = ifThenElse(false, true)
//
//  def == (x: Boolean): Boolean = ifThenElse(x, x.unary_!)
//  def != (x: Boolean): Boolean = ifThenElse(x.unary_!, x)
//
//  def < (x: Boolean): Boolean = ifThenElse(false, x)
//
//  object true extends Boolean {
//    def ifThenElse[T](t: => T, e: => T) = t
//  }
//
//  object false extends Boolean {
//    def ifThenElse[T](t: => T, e: => T) = e
//  }
//}
