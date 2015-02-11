package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P11Test extends Specification {
  
  "P11" should {
    // test goes here
    "encodeModified" in {
      P11.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_==
        List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    }
  }
}