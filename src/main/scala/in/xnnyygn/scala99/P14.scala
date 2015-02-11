package in.xnnyygn.scala99

// a template for answer
object P14 {

  // method goes here
  def duplicate[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case h :: tail => h :: h :: duplicate(tail)
  }
}