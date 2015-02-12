package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P24Test extends Specification {
  
  "P24" should {
    // test goes here
    "lotto" in {
      val n = 6
      val m = 49
      val elements = P24.lotto(n, m)
      elements.length must_== n
      elements.forall(x => x >= 1 && x < 49) must beTrue
    }
  }
}