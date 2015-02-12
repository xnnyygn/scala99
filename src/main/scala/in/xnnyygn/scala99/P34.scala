package in.xnnyygn.scala99

import scala.language.implicitConversions

// a template for answer
object P34 {

  // method goes here
  implicit def intToP34Number(n: Int): P34Number = new P34Number(n)

  class P34Number(n: Int) {
    lazy val totient: Int = (1 to n).filter(P32.gcd(_, n) == 1).length
  }

}