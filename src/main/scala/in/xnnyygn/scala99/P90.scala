package in.xnnyygn.scala99

// a template for answer
object P90 {

  // method goes here
  def placeQueens(n: Int): List[List[Int]] = {
    def candidates(queens: List[Int]): Traversable[Int] = {
      Set(1 to n: _*) -- queens.zipWithIndex.flatMap{
        case (q, i) => List(q - i - 1, q, q + i + 1)
      }
    }
    def placeQueensR(i: Int): List[List[Int]] = {
      // println(s"place queen $n $i")
      if(i == 0) List(Nil)
      else for {
        queens <- placeQueensR(i - 1)
        c <- candidates(queens)
      } yield c :: queens
    }
    placeQueensR(n)
  }

}