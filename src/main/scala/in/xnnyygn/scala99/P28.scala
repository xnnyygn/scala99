package in.xnnyygn.scala99

// a template for answer
object P28 {

  // method goes here
  def lsort[A](xs: List[List[A]]): List[List[A]] = xs.sortBy(_.length)

  def lsortFreq2[A](xs: List[List[A]]): List[List[A]] = xs.map(
    x => (x.length, x) // like word count
  ).groupBy(_._1).toList.sortBy(_._2.length).flatMap(_._2).map(_._2)

  def lsortFreq[A](xs: List[List[A]]): List[List[A]] = {
    val freqs = xs.map(_.length).groupBy(identity).map{case (k, v) => (k, v.length)}
    xs.sortWith{(l1, l2) => freqs(l1.length) < freqs(l2.length)}
  }
}