package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P50Test extends Specification {
  
  "P50" should {
    // test goes here
    "huffman" in {
      P50.huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))) must_==
        List(("a","0"), ("b","101"), ("c","100"), ("d","111"), ("e","1101"), ("f","1100"))
    }
  }
}