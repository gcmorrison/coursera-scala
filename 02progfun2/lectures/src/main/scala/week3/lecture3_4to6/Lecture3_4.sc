object Lecture3_4 {
  type Wire = Boolean

  val a, b, c = new Wire

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s, c1, c2 = new Wire
    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)
  }

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def f(a: Wire, b: Wire, c: Wire): Unit = {
    val d, e, f, g = new Wire

    inverter(a, d) // d = !a
    inverter(b, e) // e = !b
    andGate(a, e, f) // f = a & !b
    andGate(b, d, g) // g = b & !a
    orGate(f, g, c) // c = (a & !b) | (b & !a)

    // i.e. f proves that (a != b)
  }

  def inverter(input: Wire, output: Wire): Unit = ???

  def andGate(in1: Wire, in2: Wire, output: Wire): Unit = ???

  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = ???
}