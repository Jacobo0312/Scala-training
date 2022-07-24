package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if (c == 0 || c == r) then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  end pascal

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
    def balanceHelper(chars: List[Char], open: Int): Boolean = 
      if (chars.isEmpty) then open == 0
      else if (chars.head == '(') then balanceHelper(chars.tail, open + 1)
      else if (chars.head == ')') then if (open > 0) then balanceHelper(chars.tail, open - 1) else false
      else balanceHelper(chars.tail, open)
    end balanceHelper
    balanceHelper(chars, 0)
  end balance


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) then 1
    else if (money < 0) then 0
    else if (coins.isEmpty) then 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  end countChange

