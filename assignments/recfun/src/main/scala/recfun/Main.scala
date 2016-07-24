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
      def traverseChars(chars: List[Char], balanced: List[Boolean]): Boolean = {
        if (chars.isEmpty) balanced.isEmpty
        else if (chars.head == '(') traverseChars(chars.tail, false :: balanced)
        else if
          (chars.head == ')') traverseChars(chars.tail, if (balanced.isEmpty) false :: balanced else balanced.tail)
        else traverseChars(chars.tail, balanced)
      }

      traverseChars(chars, List[Boolean]())
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(money: Int, k: Int): Int = {
        if (k == 0 || money < 0) 0
        else if (money == 0) 1
        else count(money, k - 1) + count(money - coins(k - 1), k)
      }

      count(money, coins.size)
    }
  }
