package in.xnnyygn.scala99

import scala.language.implicitConversions

// a template for answer
object P35 {

  // method goes here
  implicit def intToP35Number(n: Int): P35Number = new P35Number(n)

  class P35Number(n: Int) {
    def primeFactors: List[Int] = P35.primeFactors(n)
  }

  def primeFactors(n: Int): List[Int] = {
    if(n == 1) Nil
    else primes.takeWhile(_ <= scala.math.sqrt(n)).find(n % _ == 0) match {
      case Some(k) => k :: primeFactors(n / k)
      case _ => List(n)
    }
  }

  def isPrime(n: Int): Boolean = primes.takeWhile(_ <= scala.math.sqrt(n)).forall(n % _ != 0)

  val primes = 2 #:: Stream.from(3, 2).filter(isPrime)
}