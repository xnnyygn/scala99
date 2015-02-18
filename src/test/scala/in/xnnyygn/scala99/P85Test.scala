package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P85Test extends Specification {
  
  import shared.Graph.fromString

  "P85" should {
    // test goes here
    "graph isomorphic 1" in {
      fromString("[a-g]").isIsomorphicTo(fromString("[1-2]")) must beTrue
    }
    "graph isomorphic 2" in {
      fromString("[a-b, b-c, a-c, a-d]").isIsomorphicTo(fromString("[1-3, 2-3, 3-4, 1-4]")) must beTrue
    }
    "graph isomorphic 3" in {
      fromString("[a-b, b-c, a-c, a-d]").isIsomorphicTo(fromString("[1-4, 2-3, 3-4, 1-2]")) must beFalse
    }
  }
}