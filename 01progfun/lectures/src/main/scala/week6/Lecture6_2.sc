object Lecture6_2 {
  val n = 7

  val flattened = ((1 until n) map (i => (1 until i) map (j => (i, j)))).flatten
  val flatmapped = (1 until n) flatMap (i => (1 until i) map (j => (i, j)))

  println("Flattened = " + flattened)
  println("FlatMapped = " + flatmapped)
  println("Are they equal? " + (flattened == flatmapped))

  def isPrime(n: Int) = (2 until n) forall (n % _ != 0)
  val rangesAndFilter = flatmapped filter (pair => isPrime(pair._1 + pair._2))
  println("All pairs that form a Prime number using range&filter: " + rangesAndFilter)

  case class Person(name: String, age: Int)

  val persons = List(new Person("Campbell", 32))
  for ( p <- persons if p.age > 20) yield println(p.name + " is over 20 years old")

  val looped = for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)
  println("All pairs that form a Prime number using loops: " + looped)
  println("Are ranges&filter and loops equal? " + (rangesAndFilter == looped))
}

