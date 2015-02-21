package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P96Test extends Specification {
  
  "P96" should {
    // test goes here
    "a-12a is identifier" in {
      P96.isIdentifier("a-12a") must beTrue
    }
    "-12 is not identifier" in {
      P96.isIdentifier("-12") must beFalse
    }
  }
}