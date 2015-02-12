package in.xnnyygn.scala99

import scala.language.implicitConversions

// a template for answer
object P31 {

  // method goes here
  implicit def intToP31Number(n: Int): P31bNumber = new P31bNumber(n)

  class P31Number(n: Int) {
    def isPrime: Boolean = (2 to scala.math.sqrt(n).toInt).find{x =>
      // println(s"$n / $x")
      n % x == 0
    }.isEmpty
  }

  class P31bNumber(n: Int) {
    def isPrime: Boolean = P31.isPrime(n)
  }

  def isPrime(n: Int): Boolean = primes.takeWhile(_ <= scala.math.sqrt(n)).forall(n % _ != 0)

  val primes = 2 #:: Stream.from(3, 2).filter(isPrime)
}