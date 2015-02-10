package in.xnnyygn.scala99

// a template for answer
object P04 {

  // method goes here
  def length(xs: List[_]): Int = xs match {
    case Nil => 0
    case _ :: tail => 1 + length(tail)
  }
  
}