package reductions

import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var acc = 0
    for {
      c <- chars
      if acc >= 0
    } {
      acc = acc + {
        if (c == '(') 1
        else if (c == ')') -1
        else 0
      }
    }
    acc == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): Int = {
      var index = idx
      var acc = 0
      while (index < until) {
        acc = acc + {
          if (chars(idx) == '(') 1
          else if (chars(idx) == ')') -1
          else 0
        }
        index = index + 1
      }
      acc
    }

    def reduce(from: Int, until: Int, depth: Int): Int = {
      if (until - from <= threshold) traverse(from, until)
      else {
        val mid = from + (until - from) / 2
        val (r1, r2) = parallel(reduce(from, mid, depth + 1), reduce(mid, until, depth + 1))
        if (depth == 0 && r2 > r1) -threshold else r1 + r2
      }
    }

    reduce(0, chars.length, 0) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
