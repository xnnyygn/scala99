package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P13Test extends Specification {
  
  "P13" should {
    // test goes here
    "encodeDirect" in {
      P13.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_==
        List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    }
  }
}