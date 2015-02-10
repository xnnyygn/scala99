package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P09Test extends Specification {
  
  "P09" should {
    // test goes here
    "pack" in {
      P09.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_== 
        List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    }
  }
}