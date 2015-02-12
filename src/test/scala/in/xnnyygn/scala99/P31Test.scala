package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P31Test extends Specification {
  
  "P31" should {
    // import scala.language.implicitConversions
    import P31.intToP31Number
    // test goes here
    "7 is prime" in {
      7.isPrime must beTrue
    }
    "10 is not prime" in {
      10.isPrime must beFalse
    }
  }
}