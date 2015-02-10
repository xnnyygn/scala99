package in.xnnyygn.scala99

import org.specs2.mutable._

class P01Test extends Specification {
  
  "P01 last" should {
    "last element of [1, 2, 3] is 3" in {
      P01.last(List(1, 2, 3)) must_== Some(3)
    }
    "last element of [] is None" in {
      P01.last(Nil) must_== None
    }
  }
}