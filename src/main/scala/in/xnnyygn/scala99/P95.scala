package in.xnnyygn.scala99

// a template for answer
object P95 {

  val words = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  // method goes here
  def fullWords(n: Int): String = {
    def digits(m: Int, result: List[Int]): List[Int] = {
      if(m == 0) result
      else digits(m / 10, m % 10 :: result)
    }
    digits(n, Nil).map(words).mkString("-")
  }

}