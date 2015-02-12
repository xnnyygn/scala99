package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P34Test extends Specification {
  
  "P34" should {
    // test goes here
    import P34.intToP34Number
    
    "totient of 10 is 4" in {
      10.totient must_== 4
    }
  }
}