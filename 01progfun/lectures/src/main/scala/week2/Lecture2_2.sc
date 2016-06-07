object Lecture2_2 {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum(f)(a + 1, b)
  }
  sum(x => x)(0, 5)

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }
  product(x => x)(1, 5)

  def factorial(a: Int) = product(x => x)(1, a)
  factorial(4)

  def apply(f: Int => Int, g: (Int, Int) => Int, initial: Int)(a: Int, b: Int): Int = {
    if (a > b) initial
    else g(f(a), apply(f, g, initial)(a + 1, b))
  }

  def genSum(a: Int, b: Int) = apply(x => x, (left, right) => left + right, 0)(a, b)
  genSum(0, 5)

  def genProd(a: Int, b: Int) = apply(x => x, (left, right) => left * right, 1)(a, b)
  genProd(3, 5)
}