package in.xnnyygn.scala99

// a template for answer
object P37 {

  // method goes here
  def phi(n: Int): Int = P36.primeFactorMultiplicity(n).foldLeft(1){
    case (acc, (p, m)) => acc * (p - 1) * pow(p, m - 1)
  }

  def pow(a: Int, b: Int): Int = (a, b) match {
    case (_, 0) => 1
    case (_, 1) => a
    case (1, _) => 1
    case _ => a * pow(a, b - 1)
  }

}