package recfun

object Main extends App {
  override def main(args: Array[String]) {
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
//    println(s"c == $c, r == $r")
    if (c <= 0 || r <= 0 || c == r) 1
    else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], numOpen: Int): Boolean = {
      if (chars.isEmpty) return numOpen == 0
      else if (numOpen < 0) return false
      else if (chars.head == ')') loop(chars.tail, numOpen-1)
      else if (chars.head == '(') loop(chars.tail, numOpen+1)
      else loop(chars.tail, numOpen)
    }
    loop(chars, 0)
  }

  /**
    * Exercise 3
    */
  // https://stackoverflow.com/questions/12629721/coin-change-algorithm-in-scala-using-recursion
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if(money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }
//  def countChange(money: Int, coins: List[Int]): Int = {
//    def loop(total: Int, partial: List[Int], coins: List[Int]): Int = {
//        println(s"called with $total, $partial, $coins")
//        if (total < 0) return 0
//        if (partial.sum == total) {
//          println(partial)
//          return 1
//        }
//        if (coins.isEmpty) loop(total - partial.sum, partial :+ coins.head, coins)
//        if (total - coins.head >= 0) loop(total - partial.sum, partial :+ coins.head, coins)
//        else loop(total - partial.sum, partial :+ coins.head, coins.tail)
//    }
//    loop(money: Int, List[Int](), coins.sorted(Ordering.Int.reverse))
//  }
}
