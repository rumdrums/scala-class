package week1

import scala.annotation.tailrec

object session extends App {
  def abs(x:Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  // def isGoodEnough(guess: Double, x: Double) = abs( guess * guess - x) < 0.001

  def isGoodEnough(guess: Double, x: Double) = abs( guess * guess - x) < x / 1000

  def improve(guess: Double, x: Double) = (guess + x / guess) / 2

  def sqrt(x: Double) = sqrtIter(1.0, x)

  def factorial(n: Int): Int =
    if (n == 0) 1
    else n * factorial(n-1)

  @tailrec
  def factorialTr(n: Int, total: Int = 1): Int =
    if (n == 0) total
    else factorialTr(n-1, total * n)
}