package in.xnnyygn.scala99

// a template for answer
object P39 {

  // method goes here
  def listPrimesinRange(xs: Traversable[Int]): List[Int] = xs.filter(P31.isPrime).toList

}