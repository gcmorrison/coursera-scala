package streams

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
      assert(goal == Pos(4, 7))
    }
  }

  test("Block isStanding") {
    new Level1 {
      assert(Block(Pos(1, 1), Pos(1, 1)).isStanding === true)
      assert(Block(Pos(1, 1), Pos(1, 2)).isStanding === false)
      assert(Block(Pos(1, 1), Pos(2, 1)).isStanding === false)
      assert(Block(Pos(2, 1), Pos(2, 1)).isStanding === true)
    }
  }

  test("All neighbours for level 1 of block in various configurations starting at Pos(1, 1)") {
    new Level1 {
      assert(Block(Pos(1, 1), Pos(1, 1)).neighbors.toSet === Set((Block(Pos(1, -1), Pos(1, 0)), Left), (Block(Pos(1, 2), Pos(1, 3)), Right), (Block(Pos(-1, 1), Pos(0, 1)), Up), (Block(Pos(2, 1), Pos(3, 1)), Down)))
      assert(Block(Pos(1, 1), Pos(1, 2)).neighbors.toSet === Set((Block(Pos(1, 0), Pos(1, 0)), Left), (Block(Pos(1, 3), Pos(1, 3)), Right), (Block(Pos(0, 1), Pos(0, 2)), Up), (Block(Pos(2, 1), Pos(2, 2)), Down)))
      assert(Block(Pos(1, 1), Pos(2, 1)).neighbors.toSet === Set((Block(Pos(1, 0), Pos(2, 0)), Left), (Block(Pos(1, 2), Pos(2, 2)), Right), (Block(Pos(0, 1), Pos(0, 1)), Up), (Block(Pos(3, 1), Pos(3, 1)), Down)))
    }
  }

  test("All Legal neighbours for level 1 of block in various configurations starting at Pos(1, 1)") {
    new Level1 {
      assert(Block(Pos(1, 1), Pos(1, 1)).legalNeighbors.toSet === Set((Block(Pos(1, 2), Pos(1, 3)), Right), (Block(Pos(2, 1), Pos(3, 1)), Down)))
      assert(Block(Pos(1, 1), Pos(1, 2)).legalNeighbors.toSet === Set((Block(Pos(1, 0), Pos(1, 0)), Left), (Block(Pos(1, 3), Pos(1, 3)), Right), (Block(Pos(0, 1), Pos(0, 2)), Up), (Block(Pos(2, 1), Pos(2, 2)), Down)))
      assert(Block(Pos(1, 1), Pos(2, 1)).legalNeighbors.toSet === Set((Block(Pos(1, 0), Pos(2, 0)), Left), (Block(Pos(1, 2), Pos(2, 2)), Right), (Block(Pos(0, 1), Pos(0, 1)), Up), (Block(Pos(3, 1), Pos(3, 1)), Down)))
    }
  }

  test("Neighbors with history for level 1 of block standing at various positions") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet === Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)), (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
      assert(neighborsWithHistory(Block(Pos(2, 2), Pos(2, 3)), List(Left, Up)).toSet === Set((Block(Pos(2, 1), Pos(2, 1)), List(Left, Left, Up)), (Block(Pos(2, 4), Pos(2, 4)), List(Right, Left, Up)), (Block(Pos(1, 2), Pos(1, 3)), List(Up, Left, Up)), (Block(Pos(3, 2), Pos(3, 3)), List(Down, Left, Up))))
    }
  }

  test("Only unexplored neighbors with history for level 1") {
    new Level1 {
      assert(newNeighborsOnly(Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)), (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream, Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))) === Set((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
