package in.xnnyygn.scala99

// a template for answer
object P07 {

  def flatten(xs: List[Any]): List[Any] = flattenSimple(xs)

  def flattenSimple(xs: List[Any]): List[Any] = xs.flatMap {
    case ys: List[_] => flattenSimple(ys)
    case x => List(x)
  }

  // method goes here
  def flattenR(xs: List[Any]): List[Any] = {
    def flatternR1(ys: List[Any], r: List[Any]): List[Any] = ys match {
      case Nil => r
      case (zs: List[_]) :: tail => flatternR1(tail, flatternR1(zs, Nil) ::: r)
      case y :: tail => flatternR1(tail, y :: r)
    }
    flatternR1(xs, Nil).reverse
  }

}