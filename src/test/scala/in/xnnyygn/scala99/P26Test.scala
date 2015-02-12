package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P26Test extends Specification {
  
  "P26" should {
    // test goes here
    "combinations" in {
      // 6! / 3! / 3! = 6 * 5 * 4 / 3 * 2 * 1 = 20
      P26.combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)).length must_== 20
    }
  }
}