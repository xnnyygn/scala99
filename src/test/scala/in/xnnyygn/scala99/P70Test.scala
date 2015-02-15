package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P70Test extends Specification {
  
  import shared.MTree

  "P70" should {
    // test goes here
    "node count" in {
      MTree('a', List(MTree('f'))).nodeCount must_== 2
    }
    "depth first string" in {
      MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString must_==
        "afg^^c^bd^e^^^"
    }
  }
}