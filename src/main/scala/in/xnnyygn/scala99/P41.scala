package in.xnnyygn.scala99

// a template for answer
object P41 {

  // method goes here
  def printGoldbachList(r: Traversable[Int]): Unit = goldbachList(r).foreach(printGoldbach)

  private def printGoldbach(p: (Int, Int)): Unit = p match {
    case (a, b) => {
      val n = a + b
      println(s"$n = $a + $b")
    }
  }

  /* def printGoldbachListLimited(r: Traversable[Int], l: Int): Unit = new Iterator[(Int, Int)] {
    val ri = r.toIterator
    var current = fetchNext
    private def fetchNext: Option[(Int, Int)] = ri.find(P40.goldbach(_).isDefined).flatMap(P40.goldbach(_))
    def hasNext: Boolean = current.isDefined
    def next: (Int, Int) = {
      val Some(pair) = current
      current = fetchNext
      pair
    } // a ugly implement
  }.take(l).foreach(printGoldbach)*/

  def printGoldbachListLimited(r: Traversable[Int], min: Int): Unit = goldbachList(r).filter{
    case (a, _) => a >= min
  }.foreach(printGoldbach)

  def goldbachList(r: Traversable[Int]): List[(Int, Int)] = r.flatMap(P40.goldbach).toList

}