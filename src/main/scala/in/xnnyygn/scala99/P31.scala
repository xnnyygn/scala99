package in.xnnyygn.scala99

import scala.language.implicitConversions

// a template for answer
object P31 {

  // method goes here
  implicit def intToP31Number(n: Int): P31Number = new P31Number(n)

  class P31Number(n: Int) {
    def isPrime: Boolean = (2 to scala.math.sqrt(n).toInt).find{x =>
      // println(s"$n / $x")
      n % x == 0
    }.isEmpty
  }
}