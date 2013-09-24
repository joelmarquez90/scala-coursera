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
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else if (c > r) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def inner(chars: List[Char], count: Int): Int = {
      chars match {
        case x :: tail =>
          val newCount = chars.head.charValue() match {
				            case '(' => 1
				            case ')' => if (count > 0) -1 else -2
				            case _ => 0
				          }
          inner(tail, count + newCount)
        case Nil => count
      }
    }
    inner(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (0, _) => 1
      case (mon, _) if mon < 0 => 0
      case (_, coins) if coins.isEmpty => 0
      case (mon, coins) => countChange(mon - coins.head, coins) + countChange(mon, coins.tail)
    }
  }
}
