object Lecture2_7 {
  val rational = new Rational(1, 2)
  println("First attempt: " + rational)

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  println(x + " - " + y + " - " + z + " = " + (x - y - z))

  println(x + " * " + y + " + " + z + " = " + (x * y + z))

  println(y + " + " + y + " = " + (y + y))

  println(x + " < " + y + " = " + (x < y))

  println("max of " + x + " and " + y + " = " + x.max(y))

  // Requirement checking
  //  val strange = new Rational(1, 0)
  //  println(strange + " + " + strange + " = " + strange.add(strange))

  println("Single arg constructor: " + new Rational(2))
}

Lecture2_7

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  val numer = x / gcd(x, y)

  val denom = y / gcd(x, y)

  def + (that: Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def unary_- = new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  def * (that: Rational) = new Rational(numer * that.numer, denom * that.denom)

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this < that) that else this

  override def toString = if (denom == 1) numer + "" else numer + "/" + denom
}
