package in.xnnyygn.scala99

object P03 {

  def nth[A](i: Int, xs: List[A]): Option[A] = nthRecursively(i, xs)

  def nthBuiltIn[A](i: Int, xs: List[A]): Option[A] = {
    if(i < 0 || i >= xs.length) None
    else Some(xs(i))
  }

  def nthRecursively[A](i: Int, xs: List[A]): Option[A] = (i, xs) match {
    case (_, Nil) => None
    case (0, x :: _) => Some(x)
    case (_, _ :: tail) => nthRecursively(i - 1, tail)
  }
  
}