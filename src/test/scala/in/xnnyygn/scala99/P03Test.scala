package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P03Test extends Specification {
  
  "P03" should {
    "first of [1, 2, 3] is 1" in {
      P03.nth(0, List(1, 2, 3)) must_== Some(1)
    }
    "second of [1, 2, 3] is 2" in {
      P03.nth(1, List(1, 2, 3)) must_== Some(2)
    }
    "no third of [1, 2]" in {
      P03.nth(2, List(1, 2)) must_== None
    }
  }

}