package in.xnnyygn.scala99

object P02 {

  def penultimate[A](xs: List[A]): Option[A] = xs match {
    case Nil => None
    case _ :: Nil => None
    case x :: _ :: Nil => Some(x)
    case _ :: tail => penultimate(tail)
  }

}