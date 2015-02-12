package in.xnnyygn.scala99

// a template for answer
object P22 {

  // method goes here
  def range(start: Int, end: Int): List[Int] = {
    if(start > end) Nil
    else start :: range(start + 1, end)
  }

  def range2(start: Int, end: Int): List[Int] = List.range(start, end + 1)
}