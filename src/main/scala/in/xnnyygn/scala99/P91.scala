package in.xnnyygn.scala99

// a template for answer
object P91 {

  case class Coordinate(x: Int, y: Int) {
    override def toString = s"($x, $y)"

    def availableJumps(n: Int, jumped: Set[Coordinate]): List[Coordinate] = {
      
      def availableJumps1(c: Coordinate) = List(
        Coordinate(c.x - 2, c.y - 1),
        Coordinate(c.x - 2, c.y + 1),
        Coordinate(c.x + 2, c.y - 1),
        Coordinate(c.x + 2, c.y + 1),
        Coordinate(c.x - 1, c.y - 2),
        Coordinate(c.x + 1, c.y - 2),
        Coordinate(c.x - 1, c.y + 2),
        Coordinate(c.x + 1, c.y + 2)
      ).filter(c => c.x >= 1 && c.x <= n && c.y >= 1 && c.y <= n && !jumped.contains(c))

      // Warnsdorff's Rule, start from node with least degree
      availableJumps1(this).sortBy(availableJumps1(_).length)
    }
  }

  

  // method goes here
  def knightsTour(n: Int): List[Coordinate] = {

    def tour(c: Coordinate, jumped: Set[Coordinate], path: List[Coordinate]): Either[Unit, List[Coordinate]] = {
      // println(s"tour now $c")
      if(jumped.size == n * n) Right(path)
      else c.availableJumps(n, jumped) match {
        case Nil => Left()
        case jumps => {
          for(j <- jumps) tour(j, jumped + j, j :: path) match {
            case r: Right[_, _] => return r
            case _ => ()
          }
          Left()
        }
      }
    }

    val start = Coordinate(1, 1)
    tour(start, Set(start), List(start)) match {
      case Right(path) => path
      case _ => throw new IllegalStateException("no tour")
    }
  }

  case class Frame(c: Coordinate, jumped: Set[Coordinate], path: List[Coordinate])

  def knightsTourLazy(n: Int): Stream[List[Coordinate]] = {

    def tourLazy(frames: List[Frame]): Stream[List[Coordinate]] = frames match {
      case Nil => Stream.empty
      case Frame(c, jumped, path) :: others => {
        if(jumped.size == n * n) path #:: tourLazy(others)
        else tourLazy(c.availableJumps(n, jumped).map(j => Frame(j, jumped + j, j :: path)) ::: others)
      }
    }

    val start = Coordinate(1, 1)
    tourLazy(List(Frame(start, Set(start), List(start))))
  }

  // seems to not stop at a regular time
  def knightsTourClosedSimple(n: Int): Option[List[Coordinate]] = knightsTourLazy(n).find{path =>
    val last = path.head
    // val last5 = path.take(5)
    // println(s"last position is $last, path $last5 ... (1, 1)")
    (last.x == 3 && last.y == 2) || (last.x == 2 && last.y == 3)
  }

  /* def main(args: Array[String]): Unit = {
    println(knightsTourClosedSimple(5))
  } */
}