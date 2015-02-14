package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P61Test extends Specification {
  
  import shared._

  "P61" should {
    // test goes here
    "leaf count" in {
      Node('x', Node('x'), End).leafCount must_== 1
    }
    "leaf list" in {
      Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList must_== List('b', 'd', 'e')
    }
  }
}