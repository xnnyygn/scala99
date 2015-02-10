package in.xnnyygn.scala99

// a template for answer
object P08 {

  def compress[A](xs: List[A]): List[A] = compressFunctional(xs)
  // method goes here
  def compressSimple[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case x :: tail => x :: compress(tail.dropWhile(_ == x))
  }

  def compressFunctional[A](xs: List[A]): List[A] = {
    xs.foldRight(List.empty[A]){(x, r) => 
      if(r.isEmpty || x != r.head) x :: r
      else r
    }
  }
}