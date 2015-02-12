package in.xnnyygn.scala99

// a template for answer
object P24 {

  // method goes here
  def lotto(n: Int, m: Int): List[Int] = P23.randomSelect(n, List.range(1, m + 1))

}