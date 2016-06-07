object Lecture2_6 {
  val rational = new Rational(1, 2)
  println("First attempt: " + rational)

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  println(x + " - " + y + " - " + z + " = " + x.sub(y).sub(z))

  println(x + " * " + y + " + " + z + " = " + x.mul(y).add(z))

  println(y + " + " + y + " = " + y.add(y))

  println(x + " < " + y + " = " + x.less(y))

  println("max of " + x + " and " + y + " = " + x.max(y))

  // Requirement checking
  //  val strange = new Rational(1, 0)
  //  println(strange + " + " + strange + " = " + strange.add(strange))

  println("Single arg constructor: " + new Rational(2))
}

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  val numer = x / gcd(x, y)

  val denom = y / gcd(x, y)

  def add(that: Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def neg = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) = new Rational(numer * that.numer, denom * that.denom)

  def less(that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this less that) that else this

  override def toString = if (denom == 1) numer + "" else numer + "/" + denom
}

Lecture2_6