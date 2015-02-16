package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P72Test extends Specification {
  
  import shared.MTree._

  "P72" should {
    // test goes here
    "postorder" in {
      "afg^^c^bd^e^^^".postorder must_== List('g', 'f', 'c', 'd', 'e', 'b', 'a')
    }
  }
}