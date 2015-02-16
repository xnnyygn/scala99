package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P82Test extends Specification {
  
  import shared.Graph

  "P82" should {
    // test goes here
    "find cycle" in {
      val g = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]")
      // println(g)
      g.findCycles("f").toSet must_== Set(List("f", "c", "b", "f"), List("f", "b", "c", "f"))
      // true must beTrue
    }
  }
}