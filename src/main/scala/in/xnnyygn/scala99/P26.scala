package in.xnnyygn.scala99

// a template for answer
object P26 {

  // method goes here
  def combinations[A](n: Int, xs: List[A]): List[List[A]] = {
    // take 2 from (1, 2, 3, 4)
    // 1, take 1 from (2, 3, 4)
    // 2, take 1 from (3, 4)
    // 3, take 1 from (4)
    def tails(m: Int, ys: List[A]): List[(A, List[A])] = {
      if(m == 0 || ys.isEmpty) Nil
      else {
        val h :: tail = ys
        (h, tail) :: tails(m - 1, tail)
      }
    }
    def combinationsR(m: Int, ys: List[A]): List[List[A]] = {
      if(m == 0 || ys.isEmpty) Nil
      else if(m == 1) ys.map(List(_))
      else tails(ys.length - m + 1, ys).flatMap {
        case (y, tail) => combinationsR(m - 1, tail).map(y :: _)
      }
    }
    combinationsR(n, xs)
  }

}