package org.mycode


class Rational(n: Int, d: Int) {
  require(d != 0)

  private val g = gcd(n.abs, d.abs)

  val numer: Int  = n / g
  val denom: Int  = d / g

  def this(n: Int) = this(n, 1) //auxiliary constructor
  override def toString: String = n + "/" + d

  def +(that: Rational): Rational = {
    new Rational(this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom)
  }

  def + (i: Int): Rational = {
    new Rational(numer + i * denom, denom)
  }

  def * (i: Int): Rational = {
    new Rational(numer * i, denom)
  }
  val r4  = 1 + 3;
  def *(that: Rational): Rational = {
    new Rational(numer * that.numer, denom * that.denom)
  }

  def lessThan(that: Rational) = {
    this.numer * that.denom < that.numer * this.denom
  }

  def max(that: Rational) =
    if (this.lessThan(that)) that else this

  private def gcd(a: Int, b: Int): Int =  {
    if (b == 0) a else gcd(b, a % b)
  }

}

object RationalMain extends App {
  val r = new Rational(10, 40)
  println(r.toString)

}