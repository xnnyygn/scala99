package in.xnnyygn.scala99

import scala.language.implicitConversions
// a template for answer
object P40 {

  // method goes here
  implicit def intToP40Number(n: Int): P40Number = new P40Number(n)

  class P40Number(n: Int) {
    def goldbach: Option[(Int, Int)] = P40.goldbach(n)
  }

  def goldbach(n: Int): Option[(Int, Int)] = {
    P31.primes.takeWhile(_ < n / 2).find(a => P31.isPrime(n - a)) match {
      case Some(a) => Some(a, n - a)
      case _ => None
    }
  }
}