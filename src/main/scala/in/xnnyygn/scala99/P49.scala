package in.xnnyygn.scala99

// a template for answer
object P49 {

  // method goes here
  def gray(n: Int): List[List[Int]] = {
    if(n == 0) List(Nil)
    else List(0, 1).flatMap(p => gray(n - 1).map(p :: _))
  }

  import scala.collection.mutable
  def grayMemorized(n: Int): List[List[Int]] = {
    val strings = mutable.Map[Int, List[List[Int]]](0 -> List(Nil))
    if(!strings.contains(n)) strings += (n -> List(0, 1).flatMap(p => grayMemorized(n - 1).map(p :: _)))
    strings(n)
  }

}