object Lecture2_5 {
  val rational = new Rational(1, 2)
  println("First attempt: " + rational)

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  println(x + " - " + y + " - " + z + " = " + x.sub(y).sub(z))

  println(x + " * " + y + " + " + z + " = " + x.mul(y).add(z))
}

class Rational(val numer: Int, val denom: Int) {
  def add(that: Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def neg = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) = new Rational(numer * that.numer, denom * that.denom)

  override def toString = numer + "/" + denom
}

Lecture2_5