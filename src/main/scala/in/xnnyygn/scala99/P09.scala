package in.xnnyygn.scala99

// a template for answer
object P09 {

  // method goes here
  def pack[A](xs: List[A]): List[List[A]] = pack2(xs)

  def packSimple[A](xs: List[A]): List[List[A]] = xs match {
    case Nil => Nil
    case x :: _ => xs.takeWhile(_ == x) :: packSimple(xs.dropWhile(_ == x))
  }

  def pack2[A](xs: List[A]): List[List[A]] = {
    if(xs.isEmpty) Nil
    else {
      val (prefix, remaining) = xs.span(_ == xs.head)
      prefix :: pack2(remaining)
    }
  }
}