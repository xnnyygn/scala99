package in.xnnyygn.scala99

import org.specs2.mutable._

class P02Test extends Specification {
  
  "P02 last" should {
    "penultimate element of [1, 2, 3] is 2" in {
      P02.penultimate(List(1, 2, 3)) must_== Some(2)
    }
    "penultimate element of [1] is None" in {
      P02.penultimate(List(1)) must_== None
    }
    "penultimate element of [] is None" in {
      P02.penultimate(Nil) must_== None
    }
  }
}