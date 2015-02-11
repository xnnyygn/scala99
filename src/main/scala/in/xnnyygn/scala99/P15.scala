package in.xnnyygn.scala99

// a template for answer
object P15 {

  // method goes here
  def duplicateN[A](n: Int, xs: List[A]): List[A] = xs.flatMap(x => List.fill(n)(x))

}