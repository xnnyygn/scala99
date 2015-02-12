package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P25Test extends Specification {
  
  "P25" should {
    // test goes here
    "randomPermute" in {
      val elements = List('a, 'b, 'c, 'd, 'e, 'f)
      val randomElements = P25.randomPermute(elements)
      elements.toSet must_== randomElements.toSet
    }
  }
}