package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P20Test extends Specification {
  
  "P20" should {
    // test goes here
    "removeAt" in {
      P20.removeAt(1, List('a, 'b, 'c, 'd)) must_== (List('a, 'c, 'd),'b)
    }
  }
}