package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P06Test extends Specification {
  
  "P06" should {
    // test goes here
    "[] is palindrome" in {
      P06.isPalindrome(Nil) must beTrue
    }
    "[1] is palindrome" in {
      P06.isPalindrome(List(1)) must beTrue
    }
    "[1, 2] is not palindrome" in {
      P06.isPalindrome(List(1, 2)) must beFalse
    }
    "[1, 2, 1] is palindrome" in {
      P06.isPalindrome(List(1, 2, 1)) must beTrue
    }
    "[1, 2, 2, 1] is palindrome" in {
      P06.isPalindrome(List(1, 2, 2, 1)) must beTrue
    }
  }
}