package reductions

import scala.annotation._
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

      def count(charList: Array[Char]) = {
      if (charList.isEmpty) 0
      else if (charList.head == '(') 1
      else if (charList.head == ')') -1
      else 0
    }

    def balanceIter(total: Int, rightChars: Array[Char]): Boolean = {
      val sum = total + count(rightChars)
      if (sum < 0) false
      else if (rightChars.isEmpty && total == 0) true
      else if (total >= 0 && rightChars.nonEmpty) balanceIter(sum, rightChars.tail)
      else false
    }

    balanceIter(0, chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
      ???
    }

    def reduce(from: Int, until: Int): Boolean = {
      ???
    }

    reduce(0, chars.length) == ???
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
