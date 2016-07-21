package example

/**
  * Created by oxfist on 7/21/16.
  */
object Newton {
  def sqrt(x: Double): Double = {

    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    def isGoodEnough(guess: Double): Boolean = Math.abs(guess * guess - x) / x < 0.001

    def improve(guess: Double): Double = (guess + x / guess) / 2

    sqrtIter(1.0)
  }
}
