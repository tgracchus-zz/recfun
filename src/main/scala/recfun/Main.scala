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

    def pascalIter(n: Int, pascalList: List[Int]): Int = {
      if (n == 0) {
        pascalList(c)
      } else {
        pascalIter(n - 1, computeNextPascalList(pascalList))
      }

    }

    def computeNextPascalList(pascalList: List[Int]): List[Int] = {

      def computeNextPascalListIter(i: Int, newPascalList: List[Int]): List[Int] = {
        if (i < pascalList.length + 1) {
          val previosValue: Int = if (i - 1 < 0) 0 else pascalList(i - 1)
          val nextValue: Int = if (i >= pascalList.length) 0 else pascalList(i)
          val value: Int = nextValue + previosValue

          computeNextPascalListIter(i + 1, value :: newPascalList)
        }
        else {
          newPascalList
        }
      }

      computeNextPascalListIter(0, List())
    }

    if (c <= r) {
      pascalIter(r, List(1))
    } else {
      throw new IllegalArgumentException();
    }


  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balanceIter(i: Int, balance: Int): Int = {
      if (balance >= 0 && i < chars.length) {

        chars(i) match {
          case '(' => balanceIter(i + 1, balance + 1)
          case ')' => balanceIter(i + 1, balance - 1)
          case _ => balanceIter(i + 1, balance)
        }

      } else {
        balance
      }

    }

    balanceIter(0, 0) match {
      case 0 => true
      case _ => false
    }

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    /*def countChangeInter(coins: List[Int], money: Int): Int = {
      if (money == 0) return 1
      if (coins.isEmpty || money < 0) return 0

      countChangeInter(coins, money - coins.head) + countChangeInter(coins.tail, money)
    }

    countChangeInter(coins, money)*/


    var table = Array.ofDim[Int](coins.length + 1, money + 1)

    for (coinArray <- table) coinArray(0) = 1

    for (i <- 1 until coins.length + 1) {
      for (j <- 1 until money + 1) {
        if (coins(i - 1) <= j) {
          table(i)(j) = table(i - 1)(j) + table(i)(j - coins(i - 1))
        } else {
          table(i)(j) = table(i - 1)(j)
        }
      }


    }


    table(coins.length)(money)


  }

}
