package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P16Test extends Specification {
  
  "P16" should {
    // test goes here
    "drop" in {
      P16.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
        List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    }
  }
}