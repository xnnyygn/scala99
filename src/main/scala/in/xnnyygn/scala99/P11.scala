package in.xnnyygn.scala99

// a template for answer
object P11 {

  // method goes here
  def encodeModified[A](xs: List[A]): List[Any] = P10.encode(xs).map {
    case (1, x) => x
    case y => y
  }

}