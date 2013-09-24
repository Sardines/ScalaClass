package recfun
import common._

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }	
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    (c,r) match  {
      case (0,_) => 1
      case (_,0) => 1
      case (col,row) => if (col == row) 1 else pascal(c-1, r-1) + pascal (c, r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def helper(listchar: List[Char], acc: Int): Boolean = {
       if (acc < 0) false
       else (listchar, acc) match {
         case (Nil,_) => (acc == 0)
         case (h::t,_) => {
           if (h == '(') helper(t,acc+1)
           else if (h == ')')  helper(t,acc-1) 
           else helper(t,acc)
         }
       }
    }
    helper(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money,coins) match {
      case (_,List()) => 0
      case (0,_) => 1
      case (m,h::t) => if (m < 0) 0 else countChange(m-h,coins)+countChange(m,t)    
    }
  } 
}
