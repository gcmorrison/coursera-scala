package week2.lecture2_5

/**
  * Created by campbell on 2016/07/07.
  */
class Pouring(capacity: Vector[Int]) {
  // States
  type State = Vector[Int]
  val initialState = capacity map (x => 0)
  val glasses = 0 until capacity.length
  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))
  val initialPath = new Path(Nil, initialState)
  val pathSets = from(Set(initialPath), Set(initialState))

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  def solutions(target: Int): Stream[Path] = {
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
  }

  // Moves
  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    def change(state: State): State = state updated(glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    def change(state: State): State = state updated(glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated(from, state(from) - amount) updated(to, state(to) + amount)
    }
  }

  // Paths
  class Path(history: List[Move], val endState: State) {
    def extend(move: Move) = new Path(move :: history, move change endState)

    override def toString: String = (history.reverse mkString " ") + "--> " + endState
  }

}
