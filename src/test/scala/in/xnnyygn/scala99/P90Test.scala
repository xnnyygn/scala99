package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P90Test extends Specification {
  
  "P90" should {
    // test goes here
    "eight queen 92" in {
      P90.placeQueens(8).length must_== 92
    }
  }
}