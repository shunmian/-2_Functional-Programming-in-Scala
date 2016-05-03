package recfun
import common._

object Main {

  
  

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =  {
    if(c == 0 || r == c) 1
    else pascal(c-1,r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  //(if (zero? x) max (/ 1 x)) true
  def balance(chars: List[Char]): Boolean = {
    
    def counting(chars:List[Char], acc:Int):Int = {
      if (acc < 0) -1
      else if (chars.isEmpty) acc
      else if (chars.head == '(') counting(chars.tail, acc + 1)
      else if (chars.head == ')') counting(chars.tail, acc-1)
      else counting(chars.tail,acc)
    }
    
    if (counting(chars,0) < 0) false
    else true
    
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    if(money == 0)1
    else if(money > 0 && !coins.isEmpty) countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0

  }
  
    def main(args: Array[String]) {
    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }
    
    var chars:List[Char] = "())(".toList
    if (balance(chars)) println("true")
    else println("false")
    
    var number = countChange(6,List(1,2,3))
     println("number1: " + number)
     println("hi")
  }
}
