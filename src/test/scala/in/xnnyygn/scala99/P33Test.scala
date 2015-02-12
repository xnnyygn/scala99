package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P33Test extends Specification {
  
  "P33" should {
    // test goes here
    import P33.intToP33Number

    "35 is coprime to 64" in {
      35.isCoprimeTo(64) must beTrue
    }
  }
}