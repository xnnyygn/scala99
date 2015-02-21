package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P95Test extends Specification {
  
  "P95" should {
    "175" in {
      P95.fullWords(175) must_== "one-seven-five"
    }
  }
}