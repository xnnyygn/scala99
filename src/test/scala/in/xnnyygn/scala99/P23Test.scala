package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P23Test extends Specification {
  
  "P23" should {
    // test goes here
    "randomSelect" in {
      val elements = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
      val elementSet = elements.toSet
      val subset = P23.randomSelect(3, elements)
      subset.length must_== 3
      subset.forall(elementSet.contains) must beTrue
    }
  }
}