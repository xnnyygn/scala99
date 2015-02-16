package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P71Test extends Specification {
  
  import shared.MTree
  import MTree._ // implicit conversion

  "P71" should {
    "internal path length" in {
      "afg^^c^bd^e^^^".internalPathLength must_== 9
    }
  }
}