package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P62Test extends Specification {
  
  import shared._

  "P62" should {
    // test goes here
    "internal list" in {
      Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList must_== List('a', 'c')
    }
    "at level" in {
      Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2) must_== List('b', 'c')
    }
  }
}