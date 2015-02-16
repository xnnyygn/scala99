package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P84Test extends Specification {
  
  import shared.Graph

  "P84" should {
    // test goes here
    "minimal spanning tree 1" in {
      val g = Graph.termLabel(
        List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
        List(('a', 'b', 5), ('a', 'd', 3), ('b', 'c', 2), ('b', 'e', 4),
          ('c', 'e', 6), ('d', 'e', 7), ('d', 'f', 4), ('d', 'g', 3),
          ('e', 'h', 5), ('f', 'g', 4), ('g', 'h', 1)
        )
      )
      println(g.minimalSpanningTree)
      true must beTrue
    }
    "minimal spanning tree 2" in {
      Graph.fromStringInt("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree must_==
        Some(Graph.fromStringInt("[a-b/1, b-c/2]"))
    }
  }
}