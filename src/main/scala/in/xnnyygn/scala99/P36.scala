package in.xnnyygn.scala99

import scala.language.implicitConversions

// a template for answer
object P36 {

  // method goes here
  implicit def intToP36Number(n: Int): P36Number = new P36Number(n)

  class P36Number(n: Int) {
    def primeFactorMultiplicity: Map[Int, Int] = P36.primeFactorMultiplicity(n)
  }

  def primeFactorMultiplicity(n: Int): Map[Int, Int] = P35.primeFactors(n).groupBy(identity).map{
    case (n, ns) => (n, ns.length)
  }

}