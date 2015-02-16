package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P83Test extends Specification {
  
  import shared.Graph

  "P83" should {
    // test goes here
    "spanning tree" in {
      Graph.fromString("[a-b, b-c, a-c]").spanningTrees.toSet must_==
        Set("[a-b, b-c]", "[a-c, b-c]", "[a-b, a-c]").map(Graph.fromString)
    }
    "is tree" in {
      Graph.fromString("[a-b, b-c]").isTree must beTrue
      Graph.fromString("[a-b, b-c, a-c]").isTree must beFalse
    }
    "is connected" in {
      Graph.fromString("[a, b-c]").isConnected must beFalse
      Graph.fromString("[a-b, b-c, a-c]").isConnected must beTrue
    }
  }
}