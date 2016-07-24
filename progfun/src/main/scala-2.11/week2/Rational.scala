package week2

/**
  * Created by oxfist on 7/24/16.
  */
class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private val g = gcd(x, y)

  val numer = x / g
  val denom = y / g

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def <(that: Rational): Boolean = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational): Rational = if (this < that) that else this

  def +(that: Rational): Rational = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def -(that: Rational): Rational = this + -that

  def unary_- : Rational = new Rational(-numer, denom)

  override def toString: String =
    numer + "/" + denom
}
