object Lecture6_4 {
  // Reading elements
  val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)

  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

  capitalOfCountry("US")
  capitalOfCountry get "US"
  // capitalOfCountry("Andorra") // Crashes because there is no such element
  capitalOfCountry get "Andorra"

  def showCapital(country: String) = capitalOfCountry.get(country) match {
    case Some(capital) => capital
    case None => "missing data"
  }

  showCapital("US")
  showCapital("Andorra")
  // Sorted and GroupBy
  val fruit = List("apple", "pear", "orange", "pineapple")
  fruit sortWith (_.length < _.length)
  fruit.sorted
  fruit groupBy (_.head)

  // Map Example: x^3 - 2x + 5 represented as Map(0 -> 5, 1 -> -2, 3 -> 1)
  // First implementation: Map as partial function
  class Poly1(val terms: Map[Int, Double]) {
    def +(other: Poly1) = new Poly1(terms ++ (other.terms map adjust))
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      terms get exp match {
        case Some(coeff1) => exp -> (coeff + coeff1)
        case None => exp -> coeff
      }
    }

    override def toString = {
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
    }
  }
  val p1 = new Poly1(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)) // 6.2x^5 + 4x^3 + 2x
  val p2 = new Poly1(Map(0 -> 3.0, 3 -> 7.0)) // 7x^3 + 3
  p1 + p2 // 6.2x^5 + 11x^3 + 2x + 3
  // p1.terms(7) // Crashes because there is no such polynomial

  // Second implementation: Map as total function
  class Poly2(terms0: Map[Int, Double]) {
    val terms = terms0 withDefaultValue 0.0

    def +(other: Poly2) = new Poly2(terms ++ (other.terms map adjust))

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }

    override def toString = {
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
    }
  }
  val p3 = new Poly2(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  // 6.2x^5 + 4x^3 + 2x
  val p4 = new Poly2(Map(0 -> 3.0, 3 -> 7.0)) // 7x^3 + 3
  p3 + p4 // 6.2x^5 + 11x^3 + 2x + 3
  p3.terms(7)

  // Third implementation: Simplified creation with auxiliary constructor to receive sequence
  class Poly3(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def +(other: Poly3) = new Poly3(terms ++ (other.terms map adjust))

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }

    override def toString = {
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
    }
  }
  val p5 = new Poly3(1 -> 2.0, 3 -> 4.0, 5 -> 6.2) // 6.2x^5 + 4x^3 + 2x
  val p6 = new Poly3(0 -> 3.0, 3 -> 7.0) // 7x^3 + 3
  p5 + p6 // 6.2x^5 + 11x^3 + 2x + 3
  p5.terms(7)

  // Example implementation using foldLeft
  class PolyFL(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def +(other: PolyFL) = new PolyFL((other.terms foldLeft terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
      val (exp, coeff) = term
      terms + (exp -> (terms(exp) + coeff))
    }

    override def toString = {
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
    }
  }
  val p7 = new PolyFL(1 -> 2.0, 3 -> 4.0, 5 -> 6.2) // 6.2x^5 + 4x^3 + 2x
  val p8 = new PolyFL(0 -> 3.0, 3 -> 7.0) // 7x^3 + 3
  p7 + p8 // 6.2x^5 + 11x^3 + 2x + 3
  p7.terms(7)
}