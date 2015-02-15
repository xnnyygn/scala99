package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P65Test extends Specification {
  
  import shared._

  "P65" should {
    // test goes here
    "height 1" in {
      Node(0).height must_== 1
    }
    "height 2" in {
      Node(0, Node(0), End).height must_== 2
    }
    "height 2 full" in {
      Node(0, Node(0), Node(0)).height must_== 2
    }
    "height 3" in {
      Node(0, Node(0, End, Node(0)), End).height must_== 3
    }
    "height 3 left is deeper" in {
      Node(0, Node(0, Node(0), End), Node(0)).height must_== 3
    }
    "height 3 right is deeper" in {
      Node(0, Node(0), Node(0, Node(0), Node(0))).height must_== 3
    }
    "height 5" in {
      val t = Tree.fromList(List('n','k','m','c','a','e','d','g','u','p','q'))
      println(t)
      t.height must_== 5
    }
  }
}