package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P55Test extends Specification {
  
  "P55" should {
    // test goes here
    "generate balance tree" in {
      P55.Tree.cBalanced(4, "x").length must_== 4
    }
  }
}