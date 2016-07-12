package week3.lecture3_4to6

/**
  * Created by campbell on 2016/07/11.
  */
abstract class Circuits extends Gates {
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
}
