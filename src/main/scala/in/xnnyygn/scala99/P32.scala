package in.xnnyygn.scala99

// a template for answer
object P32 {

  // method goes here
  def gcd(a: Int, b: Int): Int = {
    if(b == 0) a
    else gcd(b, a % b)
  }
}