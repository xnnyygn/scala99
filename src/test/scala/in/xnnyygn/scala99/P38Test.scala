package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P38Test extends Specification {
  
  "P38" should {
    // test goes here
    import P34.intToP34Number
    "phi equals totient" in {
      val n = 10090
      n.totient must_== P37.phi(n)
    }
  }
}