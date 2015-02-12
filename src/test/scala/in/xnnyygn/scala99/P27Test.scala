package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P27Test extends Specification {
  
  "P27" should {
    // test goes here
    "group3" in {
      P27.group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).length must_== 1260
    }
    "group" in {
      P27.group(List(2, 3, 4), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).length must_== 1260
    }
  }
}