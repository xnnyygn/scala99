package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P32Test extends Specification {
  
  "P32" should {
    // test goes here
    "gcd(36, 63) = 9" in {
      P32.gcd(36, 63) must_== 9
    }
  }
}