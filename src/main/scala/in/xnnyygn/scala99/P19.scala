package in.xnnyygn.scala99

// a template for answer
object P19 {

  // method goes here
  def rotate[A](n: Int, xs: List[A]): List[A] = {
    if(n == 0) xs
    else if(n < 0) rotate(n + xs.length, xs)
    else {
      val (init, tail) = xs.splitAt(n)
      tail ::: init
    }
  }

}