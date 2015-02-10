object P01 {

  def last[A](xs: List[A]): Option[A] = lastRecursively(xs)

  def lastBuiltIn[A](xs: List[A]): Option[A] = xs match {
    case Nil => None
    case _ => Some(xs.last)
  }

  def lastRecursively[A](xs: List[A]): Option[A] = xs match {
    case Nil => None
    case x :: Nil => Some(x)
    case _ :: xs => lastRecursively(xs)
  }

}