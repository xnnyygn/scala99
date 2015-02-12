package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P40Test extends Specification {
  
  "P40" should {
    // test goes here
    import P40.intToP40Number

    "goldbach of 28 is 5 and 23" in {
      28.goldbach must_== Some((5, 23))
    }
  }
}