package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P49Test extends Specification {
  
  "P49" should {
    // test goes here
    "gray 3 should be List(000, 001, 011, 010, 110, 111, 101, 100)" in {
      P49.grayMemorized(3).map(_.mkString).toSet must_== List("000", "001", "011", "010", "110", "111", "101", "100").toSet
    }
  }
}