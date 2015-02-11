package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P14Test extends Specification {
  
  "P14" should {
    // test goes here
    "duplicate" in {
      P14.duplicate(List('a, 'b, 'c, 'c, 'd)) must_==
        List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    }
  }
}