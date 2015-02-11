package in.xnnyygn.scala99

// a template for answer
object P16 {

  // method goes here
  def drop[A](n: Int, xs: List[A]): List[A] = 
    xs.zipWithIndex.filterNot(t => (t._2 + 1) % n == 0).map(_._1)

}