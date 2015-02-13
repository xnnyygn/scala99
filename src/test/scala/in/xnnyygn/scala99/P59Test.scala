package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P59Test extends Specification {
  
  import shared.Tree

  "P59" should {
    // test goes here
    "hbal tree 3 should be 15" in {
      Tree.hbalTrees3(3, "x").length must_== 15
    }
  }
}