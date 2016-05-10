package funsets

object Main extends App {
  
  if(true) println("true")
   println("true2")

  //the compound data is only composed of data
  class Rational(x:Int,y:Int){  
    require(y !=0,"demoninator must be non-zero")
		val numer = x/gcd(x,y)
		val denomi= y/gcd(x,y)
		private def gcd(a:Int, b:Int):Int = if(b==0) a else gcd(b,a%b)
  }
  
  
  // some compound procedure is defined outside "Rational" data  
  def toString(r: Rational) = r.numer + "/" + r.denomi

  def makeRationl(x: Int) = new Rational(x, 1)
  def neg(r: Rational) = new Rational(-r.numer, r.denomi)

  def add(r1: Rational, r2: Rational): Rational = {
    new Rational(r1.numer * r2.denomi + r1.denomi * r2.numer, r1.denomi * r2.denomi)
  }

  def sub(r1: Rational, r2: Rational): Rational = add(r1, neg(r2))

  def less(r1: Rational, r2: Rational): Boolean = r1.numer * r2.denomi - r1.denomi * r2.numer < 0
  def max(r1: Rational, r2: Rational) = if (less(r1, r2)) r2 else r1
  
  
  //the compound procedure is used to react to the compound data
  val r1:Rational = new Rational(4,5)  
  val r2:Rational = new Rational(3,4)
  //for compound data, the interface between usage and implementation is "new" constructor
  
  
  val r3 = add(r1,r2)
  println(toString(r3))
  
  val r4 = sub(r1,r2)
  println(toString(r4))
    
  val r5 = max(r1,r2)
  println(toString(r5))
  
  
  abstract class Boolean{
    
    def ifThenElse[T](e1: => T, e2: => T): T
    
    def && (b2: => Boolean): Boolean = ifThenElse(b2,false)
    def || (b2: => Boolean): Boolean = ifThenElse(true,b2)
    def unary_!: Boolean             =  ifThenElse(false,true)
    
    def == (b2: Boolean): Boolean = ifThenElse(b2, b2.unary_!)
    def != (b2: Boolean): Boolean = ifThenElse(b2.unary_!, b2)
    
   
  }
  
  object true extends Boolean{
    def ifThenElse[T](e1: => T, e2: => T): T = e1
  }
  
  object false extends Boolean{
    def ifThenElse[T](e1: => T, e2: => T): T = e2
  }
  
  val b1 = new True()
  val b2 = new False()
  
  b1.ifThenElse(println("b1 is true"),println("b1 is false"))
  b2.ifThenElse(println("b2 is true"),println("b2 is false"))
  
  b1.&&(b2)
  b1.||(b2)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
