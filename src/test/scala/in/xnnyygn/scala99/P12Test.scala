package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P12Test extends Specification {
  
  "P12" should {
    // test goes here
    "decode" in {
      P12.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) must_== 
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    }
  }
}