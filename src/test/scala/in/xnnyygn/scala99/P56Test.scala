package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P56Test extends Specification {
  
  import shared._

  "P56" should {
    // test goes here
    "Node('a', Node('b'), Node('c')) is symmetric" in {
      Node('a', Node('b'), Node('c')).isSymmetric must beTrue
    }
    "Node('a', Node('b'), End) is not symmetric" in {
      Node('a', Node('b'), End).isSymmetric must beFalse
    }
  }
}