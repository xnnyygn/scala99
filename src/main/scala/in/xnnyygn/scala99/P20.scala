package in.xnnyygn.scala99

// a template for answer
object P20 {

  def removeAt[A](n: Int, xs: List[A]): (List[A], A) = {
    if(n < 0) throw new NoSuchElementException
    else (n, xs) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, h :: tail) => (tail, h)
      case (_, h :: tail) => {
        val (l, x) = removeAt(n - 1, tail)
        (h :: l, x)
      }
    }
  }

  def removeAt3[A](n: Int, xs: List[A]): (List[A], A) = {
    def removeAtR(m: Int, ys: List[A], prefix: List[A]): (List[A], A) = (m, ys) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, y :: tail) => (prefix.reverse ::: tail, y)
      case (_, y :: tail) => removeAtR(m - 1, tail, y :: prefix)
    }
    removeAtR(n, xs, Nil)
  }

  // method goes here
  def removeAt2[A](n: Int, xs: List[A]): (List[A], A) = {
    if(n < 0 || n >= xs.length) throw new NoSuchElementException
    else {
      val (init, tail) = xs.splitAt(n)
      (init ::: tail.tail, tail.head)
    }
  }
}