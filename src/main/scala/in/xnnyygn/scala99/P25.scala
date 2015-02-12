package in.xnnyygn.scala99

import scala.reflect.ClassTag // ClassTag is required for List#toArray

// a template for answer
object P25 {

  // method goes here
  def randomPermute[A: ClassTag](xs: List[A]): List[A] = randomPermuteFisherYates(xs)

  def randomPermuteFisherYates[A: ClassTag](xs: List[A]): List[A] = {
    def swap(array: Array[A], i: Int, j: Int): Unit = {
      val t = array(i)
      array.update(i, array(j))
      array.update(j, t)
    }
    val random = new scala.util.Random
    val array = xs.toArray
    for(n <- List.range(xs.length - 1, 1, step = -1)) {
      val i = random.nextInt(n)
      swap(array, i, n)
    }
    array.toList
  }

  // TODO http://okmij.org/ftp/Haskell/perfect-shuffle.txt
  def randomPermuteFunctional[A](xs: List[A]): List[A] = throw new UnsupportedOperationException

  def randomPermuteSimple[A](xs: List[A]): List[A] = P23.randomSelect(xs.length, xs)

}