package in.xnnyygn.scala99

// a template for answer
object P12 {

  // method goes here
  def decode[A](xs: List[(Int, A)]): List[A] = xs.flatMap{
    case (n, x) => List.fill(n)(x)
  }

}