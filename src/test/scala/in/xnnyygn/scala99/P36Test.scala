package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P36Test extends Specification {
  
  "P36" should {
    // test goes here
    import P36.intToP36Number
    "315 prime factors grouped [(3, 2), (5, 1), (7, 1)]" in {
      315.primeFactorMultiplicity must_== Map(3 -> 2, 5 -> 1, 7 -> 1)
    }
  }
}