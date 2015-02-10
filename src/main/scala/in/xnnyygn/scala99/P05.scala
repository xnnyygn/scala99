package in.xnnyygn.scala99

// a template for answer
object P05 {

  // method goes here
  def reverse[A](xs: List[A]): List[A] = reverseFold(xs)

  def reverse1[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case _ => xs.last :: reverse(xs.init)
  }

  def reverseFold[A](xs: List[A]): List[A] = 
    xs.foldLeft(List.empty[A])((r, x) => x :: r)
    
}