package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P18Test extends Specification {
  
  "P18" should {
    // test goes here
    "slice" in {
      P18.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
        List('d, 'e, 'f, 'g)
    }
  }
}