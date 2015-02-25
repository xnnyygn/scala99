package in.xnnyygn.scala99

// a template for answer
object P94 {

  case class Edge(v1: Int, v2: Int) {
    override def toString = s"$v1-$v2"
  }

  // method goes here
  def kRegular(n: Int, k: Int): List[List[Edge]] = {
    val vs = List.range(1, n + 1)
    def kRegularR(cv: Int, vd: Map[Int, Int], es: List[Edge]): List[List[Edge]] = {
      println(s"k regular $cv $vd $es")
      if(cv > n) List(es)
      else {
        val d = vd.getOrElse(cv, 0)
        if(d > k) Nil // illegal solution
        else if(d == k) kRegularR(cv + 1, vd, es)
        else {
          vs.filter(_ != cv).combinations(k - d).toList.flatMap{vs2 =>
            if(vs2.exists(v => vd.getOrElse(v, 0) >= k)) Nil
            else kRegularR(cv + 1, 
              vd ++ vs2.map(v => (v, vd.getOrElse(v, 0) + 1)) + (cv -> k), 
              es ::: vs2.map(Edge(cv, _))
            )
          }
        }
      }
    }
    kRegularR(1, Map(), Nil)
  }

  /* def main(args: Array[String]): Unit = {
    println(kRegular(3, 2))
  } */

}