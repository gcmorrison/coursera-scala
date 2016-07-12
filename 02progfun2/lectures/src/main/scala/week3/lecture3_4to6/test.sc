import week3.lecture3_4to6.{Circuits, Parameters}

object test {

  val in1, in2, sum, carry = new Wire

  import sim._

  object sim extends Circuits with Parameters

  halfAdder(in1, in2, sum, carry)
  probe("sum", sum)
  probe("carry", carry)

  in1 setSignal true
  run()

  in2 setSignal true
  run()

  in1 setSignal false
  run()

}