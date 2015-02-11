package in.xnnyygn.scala99

// a template for answer
object P13 {

  // method goes here
  def encodeDirect[A](xs: List[A]): List[(Int, A)] = xs match {
    case Nil => Nil
    case h :: _ => {
      val (prefix, remaining) = xs.span(_ == h)
      (prefix.length, h) :: encodeDirect(remaining)
    }
  }
}