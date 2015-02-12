package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P39Test extends Specification {
  
  "P39" should {
    // test goes here
    "prime range of 7 to 31 is List(7, 11, 13, 17, 19, 23, 29, 31)" in {
      P39.listPrimesinRange(7 to 31) must_== List(7, 11, 13, 17, 19, 23, 29, 31)
    }
  }
}