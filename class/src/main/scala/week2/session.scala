package week2

object session extends App {

  def sum1(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a+1, f(a) + acc)
    }
    loop(a, 0)
  }

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
      if (a > b) 0
      else f(a) + sum(f)(a+1, b)
  }

  def product(f: Int => Int, a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f, a+1, b)
  }

  def productCurried(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * productCurried(f)(a+1, b)
  }

  def factorial(x: Int): Int = productCurried(y => y)(1, x)

  // to generalize both product and sum, you wind up 
  // with the same thing as mapreduce:
  def mapReduce(f: Int => Int, g: (Int, Int) => Int, zeroVal: Int)(a: Int, b: Int): Int = {
    if ( a > b ) zeroVal
    else g(f(a), mapReduce(f, g, zeroVal)(a+1, b))
  }
}
