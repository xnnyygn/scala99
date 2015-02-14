package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P60Test extends Specification {
  
  import shared.Tree

  "P60" should {
    // test goes here
    "min hbal nodes of 3 is 4" in {
      Tree.minHbalNodes(3) must_== 4
    }
    "max hbal height of 4 is 3" in {
      Tree.maxHbalHeight(4) must_== 3
    }
    "hbalTreesWithNodes 4" in {
      Tree.hbalTreesWithNodes(4, "x").length must_== 4
    }
  }
}