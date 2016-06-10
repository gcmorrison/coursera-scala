trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }

  def show: String = this match {
    case Number(n) => n.toString
    case Sum(e1, e2) => e1.show + " + " + e2.show
  }
}

case class Number(n: Int) extends Expr {
}

case class Sum(e1: Expr, e2: Expr) extends Expr {
}

/* TODO:
  Add case classes Var for variables x and Prod for products x * y as discussed
  previously. Change your show function so that it also deals with products.
  Pay attention you get operator precedence right but to use as few
  parentheses as possible.

  Example:
  Sum(Prod(2, Var(”x”)), Var(”y”))
  should print as “2 * x + y”. But
  Prod(Sum(2, Var(”x”)), Var(”y”))
  should print as "(2 * x) + y".
*/

object Lecture4_6 {

  val expr = Sum(Number(12), Number(25))
  println(expr.show + " = " + expr.eval)
}

Lecture4_6