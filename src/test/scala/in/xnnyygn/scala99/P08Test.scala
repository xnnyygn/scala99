package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P08Test extends Specification {
  
  "P08" should {
    // test goes here
    "compress" in {
      P08.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_== List('a, 'b, 'c, 'a, 'd, 'e)
    }
  }
}