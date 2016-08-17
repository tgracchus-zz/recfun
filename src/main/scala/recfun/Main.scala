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
        if (i < pascalList.length + 1){
          val previosValue : Int = if (i - 1 < 0) 0 else pascalList(i-1)
          val nextValue : Int = if (i >= pascalList.length) 0 else pascalList(i)
          val value : Int = nextValue + previosValue

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
  def balance(chars: List[Char]): Boolean = ???

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
