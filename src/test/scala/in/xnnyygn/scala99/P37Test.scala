package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P37Test extends Specification {
  
  "P37" should {
    // test goes here
    "phi(315) should be 4" in {
      P37.phi(315) must_== 144
    }
  }
}