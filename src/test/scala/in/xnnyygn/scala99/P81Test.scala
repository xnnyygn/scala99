package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P81Test extends Specification {
  
  import shared.Digraph

  "P81" should {
    // test goes here
    "find paths 1" in {
      Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q").toSet must_==
        Set(List("p", "q"), List("p", "m", "q"))
    }
    "find paths 2" in {
      Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "k") must_== Nil
    }
  }
}