package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P04Test extends Specification {
  
  "P04" should {
    // test goes here
    "length of Nil is 0" in {
      P04.length(Nil) must_== 0
    }
    "length of [1] is 1" in {
      P04.length(List(1)) must_== 1
    }
    "length of [1, 2, 3] is 3" in {
      P04.length(List(1, 2, 3)) must_== 3
    }
  }
}