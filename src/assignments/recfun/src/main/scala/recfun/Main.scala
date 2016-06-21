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
    def pascal(c: Int, r: Int): Int =
      if (r < 2 || c == 0 || c == r) 1 else r

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(parenthesis: List[Char], chars: List[Char]): List[Char] =
        if (chars.isEmpty) parenthesis
        else if (chars.head == '(') loop(chars.head::parenthesis, chars.tail)
        else if (chars.head == ')' && parenthesis.isEmpty) chars.head::parenthesis
        else if (chars.head == ')' && parenthesis.head == '(') loop(parenthesis.tail, chars.tail)
        else if (chars.head == ')') chars.head::parenthesis
        else loop(parenthesis, chars.tail)

      loop(List(), chars).isEmpty
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
