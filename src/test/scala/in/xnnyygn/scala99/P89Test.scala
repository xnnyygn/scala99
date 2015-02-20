package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P89Test extends Specification {
  
  import shared.Graph

  "P89" should {
    // test goes here
    "bigraph 1" in {
      Graph.fromString("[a-b, b-c, c-a]").isBipartite must beFalse
    }
     "bigraph 2" in {
      Graph.fromString("[a-b, b-c, d]").isBipartite must beTrue
    }
     "bigraph 3" in {
      Graph.fromString("[a-b, b-c, d, e-f, f-g, g-e, h]").isBipartite must beFalse
    }
  }
}