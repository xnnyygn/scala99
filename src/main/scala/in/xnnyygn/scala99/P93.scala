package in.xnnyygn.scala99

// a template for answer
object P93 {

  sealed trait Expr {
    def calculate: Double
  }
  case class Number(n: Int) extends Expr {
    def calculate = n
    override def toString = n.toString
  }
  case class ArithExpr(left: Expr, op: Char, right: Expr) extends Expr {
    def calculate = op match {
      case '+' => left.calculate + right.calculate
      case '-' => left.calculate - right.calculate
      case '*' => left.calculate * right.calculate
      case '/' => left.calculate / right.calculate // TODO check right
    }
    override def toString = s"($left$op$right)"
  }
  case class Equation(left: Expr, right: Expr) extends Expr {
    def calculate = throw new UnsupportedOperationException
    override def toString = s"$left=$right"
  }

  // method goes here
  def puzzle(ns: List[Int]): List[Expr] = {
    def allArithExpr(a: Expr, b: Expr): List[Expr] = List(
      ArithExpr(a, '+', b),
      ArithExpr(a, '-', b),
      ArithExpr(a, '*', b),
      ArithExpr(a, '/', b)
    )
    def puzzleR(es: List[Expr]): List[Expr] = {
      // println(s"process $es")
      es match {
        case a :: b :: Nil => allArithExpr(a, b)
        case a :: b :: c :: Nil => (allArithExpr(a, b).map(List(_, c)) ::: allArithExpr(b, c).map(List(a, _))).flatMap(puzzleR)
        case a :: b :: c :: d :: Nil => (
          allArithExpr(a, b).map(List(_, c, d)) ::: 
          allArithExpr(b, c).map(List(a, _, d)) :::
          allArithExpr(c, d).map(List(a, b, _))).flatMap(puzzleR)
        case _ => throw new IllegalStateException
      }
    }
    puzzleR(ns.tail.map(Number(_))).filter(_.calculate == ns.head).map(Equation(Number(ns.head), _)) :::
      puzzleR(ns.init.map(Number(_))).filter(_.calculate == ns.last).map(Equation(_, Number(ns.last)))
  }

  /* def main(args: Array[String]): Unit = {
    puzzle(List(2,3,5,7,11)).foreach(println)
  } */

}