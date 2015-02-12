package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P22Test extends Specification {
  
  "P22" should {
    // test goes here
    "range" in {
      P22.range(4, 9) must_== List(4, 5, 6, 7, 8, 9)
    }
  }
}