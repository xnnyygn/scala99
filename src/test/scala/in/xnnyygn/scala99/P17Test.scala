package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P17Test extends Specification {
  
  "P17" should {
    // test goes here
    "split" in {
      P17.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
        (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    }
  }
}