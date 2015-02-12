package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P21Test extends Specification {
  
  "P21" should {
    // test goes here
    "insertAt" in {
      P21.insertAt('new, 1, List('a, 'b, 'c, 'd)) must_== List('a, 'new, 'b, 'c, 'd)
    }
  }
}