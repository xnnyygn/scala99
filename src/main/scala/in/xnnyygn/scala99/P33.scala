package in.xnnyygn.scala99

import scala.language.implicitConversions
// a template for answer
object P33 {

  // method goes here
  implicit def intToP33Number(n: Int): P33Number = new P33Number(n)

  class P33Number(n: Int) {
    def isCoprimeTo(m: Int): Boolean = P32.gcd(n, m) == 1
  }

}