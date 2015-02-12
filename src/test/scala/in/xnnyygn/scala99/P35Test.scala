package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P35Test extends Specification {
  
  "P35" should {
    // test goes here
    import P35.intToP35Number
    "factors of 315 is [3, 3, 5, 7]" in {
      315.primeFactors must_== List(3, 3, 5, 7)
    }
  }

}