package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P57Test extends Specification {
  
  import shared._

  "P57" should {
    // test goes here
    "binary search tree 5, 3, 18, 1, 4, 12, 21 should be symmetric" in {
      Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric must beTrue
    }
    "binary search tree 3, 2, 5, 7, 4 is not symmetric" in {
      Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric must beFalse
    }
  }
}