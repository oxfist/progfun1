package example

/**
  * Created by oxfist on 7/21/16.
  */
object Factorial {

  def factorial(x: Int): Int = {
    def tailRecFactorial(accum: Int, x: Int): Int = {
      if (x <= 1) accum else tailRecFactorial(accum * x, x - 1)
    }

    tailRecFactorial(1, x)
  }
}
