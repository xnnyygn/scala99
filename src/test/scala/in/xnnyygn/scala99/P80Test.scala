package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P80Test extends Specification {
  
  import shared._

  "P80" should {
    // test goes here
    "graph from string" in {
      val g = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]")
      // println(g)
      val (nodes, edges) = g.toTermForm
      nodes.toSet must_== Set("d", "k", "h", "c", "f", "g", "b")
      edges must_== List(("h","g",()), ("k","f",()), ("f","b",()), ("g","h",()), ("f","c",()), ("b","c",()))
    }
    /* "digraph from string" in {
      val g = Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]")
      // val expectAdj = List(("m",List(("q",7))), ("p",List(("m",5), ("q",9))), ("k",List()), ("q",List()))
      println(g)
      println(g.toAdjacentForm)
      true must beTrue
    } */
  }
}