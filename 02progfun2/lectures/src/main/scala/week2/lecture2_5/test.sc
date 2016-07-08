import week2.lecture2_5.Pouring

object test {
  val problem = new Pouring(Vector(4, 9))
  problem.moves

  problem.pathSets.take(3).toList

  problem.solutions(6)
}
