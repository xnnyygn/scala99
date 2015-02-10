package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P05Test extends Specification {
  
  "P05" should {
    // test goes here
    "reverse of Nil is Nil" in {
      P05.reverse(Nil) must_== Nil
    }
    "reverse of [1] is [1]" in {
      P05.reverse(List(1)) must_== List(1)
    }
    "reverse of [1, 2] is [2, 1]" in {
      P05.reverse(List(1, 2)) must_== List(2, 1)
    }
  }
}