package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P15Test extends Specification {
  
  "P15" should {
    // test goes here
    "dupliateN" in {
      P15.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) must_==
        List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    }
  }
}