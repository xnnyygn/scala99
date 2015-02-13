package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P58Test extends Specification {
  
  import shared._

  "P58" should {
    // test goes here
    "symmetric balanced trees of 5 has 2" in {
      Tree.symmetricBalancedTrees(5, "x").length must_== 2
    }
  }
}