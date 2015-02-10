package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P07Test extends Specification {
  
  "P07" should {
    // test goes here
    "flatten" in {
      P07.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) must_== List(1, 1, 2, 3, 5, 8)
    }
  }
}