package in.xnnyygn.scala99

// a template for answer
object P23 {

  lazy val random = scala.util.Random

  // method goes here
  def randomSelect[A](n: Int, xs: List[A]): List[A] = {
    if(n <= 0 || xs.isEmpty) Nil
    else {
      val i = random.nextInt(xs.length)
      xs(i) :: randomSelect(n - 1, P20.removeAt(i, xs)._1)
    }
  }

}