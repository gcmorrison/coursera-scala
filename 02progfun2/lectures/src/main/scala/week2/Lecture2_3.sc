object Lecture2_3 {
  // Lazy valuation exercise
  def expr = {
    val x = {
      print("x ");
      1
    }
    lazy val y = {
      print("y ");
      2
    }
    def z = {
      print("z ");
      3
    }
    z + y + x + z + y + x
  }

  expr

  // Proving that it's better through evaluation
  /*
    def streamRange(lo: Int, hi: Int): Stream[Int] =
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))

  (streamRange(1000, 10000) filter isPrime) apply 1

  1)
  --> (if (1000 >= 10000) empty
        else 1000 #:: streamRange(1000 + 1, 10000)
      ).filter(isPrime).apply(1)

  2)
  --> (1000 #:: streamRange(1000 + 1, 10000)).filter(isPrime).apply(1)
  --> abbreviate (1000 #:: streamRange(1000 + 1, 10000)) to C1
  --> C1.filter(isPrime).apply(1)

  3)
  --> (if (C1.isEmpty) C1
        else if (isPrime(C1.head)) C1.head #:: C1.tail.filter(isPrime)
        else C1.tail.filter(isPrime))
      ).apply(1)

  4)
  --> (if (isPrime(C1.head)) C1.head #:: C1.tail.filter(isPrime)
        else C1.tail.filter(isPrime))
      ).apply(1)

  5)
  --> (if (false) C1.head #:: C1.tail.filter(isPrime)
        else C1.tail.filter(isPrime))
      ).apply(1)

  6)
  --> C1.tail.filter(isPrime).apply(1)
  ==>> streamRange(1001, 10000).filter(isPrime).apply(1)

  ...

  7) First prime in range 1000-10000)
  ==>> streamRange(1009, 10000).filter(isPrime).apply(1)
  --> abbreviate (1009 #:: streamRange(1009 + 1, 10000)) to C2
  --> C2.filter(isPrime).apply(1)

  8) Expanding from definition of Filter
  ==> (1009 #:: C2.tail.filter(isPrime)).apply(1)

  9) Expanding from definition of apply
  --> if (1 == 0) (1009 #:: C2.tail.filter(isPrime)).head
      else (1009 #:: C2.tail.filter(isPrime)).tail.apply(0)

  10) From above else block
  --> C2.tail.filter(isPrime).apply(0)

  11)
  --> streamRange(1010, 10000).filter(isPrime).apply(0)

  ...

  12)
  ==>> streamRange(1013, 10000).filter(isPrime).apply(0)
  --> abbreviate (1013 #:: streamRange(1013 + 1, 10000)) to C3
  --> C3.filter(isPrime).apply(0)

  13) Expanding from definition of Filter
  ==> (1013 #:: C3.tail.filter(isPrime)).apply(0)

  14) expanding from definition of apply
  --> if (0 == 0) (1013 #:: C3.tail.filter(isPrime)).head
      else (1013 #:: C3.tail.filter(isPrime)).tail.apply(0)
  --> (1013 #:: C3.tail.filter(isPrime)).head
  --> 1013
  */
}