package recfun


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
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def process(chars: List[Char], left: Int): Int = {
      if (chars.isEmpty) left
      else {
        chars.head match {
          case '(' => process(chars.tail, left + 1)
          case ')' => if (left > 0) process(chars.tail, left - 1) else process(List.empty, -1)
          case _ => process(chars.tail, left)
        }
      }
    }
    process(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else {
      val reversed = coins.sorted.reverse
      if (reversed.isEmpty) 0 else countChange(money, reversed.tail) + countChange(money - reversed.head, reversed)
    }
  }
}
