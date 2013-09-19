package recfun
import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1. This function computes the elements of Pascals triangle.
   * @param c column of the triangle to compute.
   * @param r row of the triangle to compute.
   * @returns the (c, r) element of the triangle.
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else if (c > r) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2. This function verifies the balancing of parentheses in a string.
   * @param chars the list of chars to check if it's balanced.
   * @returns true if it's balanced, false if it's not.
   */
  def balance(chars: List[Char]): Boolean = {
    /**
     * This is the inner balance function, where the count increases or
     * decreases, recalculated by recalculateCount, matching pair of
     * parentheses.
     * @param chars the list of chars to balance.
     * @param count increases or decreases the number of parentheses.
     * founded. 0 for correct balancing.
     * @returns the final count of matching parentheses. If it's 0, chars
     * is balanced.
     */
    @tailrec
    def inner(chars: List[Char], count: Int): Int = {
      chars match {
        case x :: tail =>
          val newCount = recalculateCount(chars, count)
          inner(tail, count + newCount)
        case Nil => count
      }
    }
    inner(chars, 0) == 0
  }

  /**
   * This function recalculates the count, adding or substracting to the original count
   * depending on the first element of the list of chars.
   * @param chars the list of chars to check the first element.
   * @param count the original count. It's used to check if already exists an
   * open parentheses (count > 0) in the chars.
   * @returns the newCount to pass to the recursive balance function.
   */
  def recalculateCount(chars: List[Char], count: Int): Int = {
    chars.head.charValue() match {
      case '(' => 1
      case ')' => if (count > 0) -1 else -2
      case _ => 0
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money > 0) {
      coins match {
        case x :: tail => 0
        case Nil => 0
      }
    } else 0
  }
}
