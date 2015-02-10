package in.xnnyygn.scala99

// a template for answer
object P10 {

  // method goes here
  def encode[A](xs: List[A]): List[(Int, A)] = P09.pack(xs).map(ys => (ys.length, ys.head))

}