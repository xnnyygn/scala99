package in.xnnyygn.scala99

// a template for answer
object P21 {

  // method goes here
  def insertAt[A](x: A, n: Int, xs: List[A]): List[A] = (n, xs) match {
    case (0, _) => x :: xs
    case (_, Nil) => throw new IllegalArgumentException(s"cannot insert at $n in Nil") // for compiler warning
    case (_, h :: tail) => h :: insertAt(x, n - 1, tail)
  }

}