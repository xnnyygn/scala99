package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P19Test extends Specification {
  
  "P19" should {
    // test goes here
    "rotate positive" in {
      P19.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
        List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    }
    "rotate negative" in {
      P19.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must_==
        List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    }
  }
}