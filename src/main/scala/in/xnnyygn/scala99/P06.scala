package in.xnnyygn.scala99

// a template for answer
object P06 {

  // method goes here
  def isPalindrome(xs: List[_]): Boolean = isPalindromeSimple(xs)

  def isPalindromeSimple(xs: List[_]): Boolean = xs == xs.reverse
}