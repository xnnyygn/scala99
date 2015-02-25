package in.xnnyygn.scala99

// a template for answer
object P97 {

  val oneToNineSet = Set(1 to 9:_*)

  class Board(
    underlying: Map[(Int, Int), Int],
    hline: Map[Int, Set[Int]],
    vline: Map[Int, Set[Int]],
    squares: Map[(Int, Int), Set[Int]]
    ) {

    private def locationInSquare(x: Int, y: Int): (Int, Int) = ((x / 3) * 3, (y / 3) * 3)

    def calculate: Map[(Int, Int), Set[Int]] = {
      underlying.filter(_._2 == 0).map{
        case ((x, y), _) => ((x, y), oneToNineSet -- hline(x) -- vline(y) -- squares(locationInSquare(x, y)))
      }
    }

    def fill(x: Int, y: Int, v: Int): Board = {
      println(s"fill board ($x, $y) with $v")
      if(underlying.get((x, y)) == Some(v)) this
      else {
        val squareXy = locationInSquare(x, y)
        new Board(
          underlying + ((x, y) -> v), 
          hline + (x -> (hline(x) + v)), 
          vline + (y -> (vline(y) + v)), 
          squares + (squareXy -> (squares(squareXy) + v))
        )
      }
    }

    def fill(m: Map[(Int, Int), Int]): Board = {
      println("fill board with " + m)
      if(m.isEmpty) this
      else {
        new Board(
          underlying ++ m,
          Board.hline(m).map(t => (t._1, hline(t._1) ++ t._2)),
          Board.vline(m).map(t => (t._1, vline(t._1) ++ t._2)),
          Board.squares(m).map(t => (t._1, squares(t._1) ++ t._2))
        )
      }
    }

    def inspect: String = {
      s"=======board=======\n$this\n\n" + 
      s"=======hline=======\n$hline\n\n" +
      s"=======vline=======\n$vline\n\n" +
      s"=======squares=======\n$squares\n"
    }
    override def toString = List.range(0, 81).map(i => underlying((i % 9, i / 9)) match {
      case 0 => " "
      case n => n.toString
    }).grouped(9).mkString("\n")
  }

  object Board {

    def hline(board: Map[(Int, Int), Int]): Map[Int, Set[Int]] = vhline(board)((xy, i) => xy._1 == i)
    def vline(board: Map[(Int, Int), Int]): Map[Int, Set[Int]] = vhline(board)((xy, i) => xy._2 == i)
    def vhline(board: Map[(Int, Int), Int])(f: ((Int, Int), Int) => Boolean): Map[Int, Set[Int]] = {
      List.range(0, 9).map(i => (i, board.filter(t => t._2 != 0 && f(t._1, i)).map(_._2).toSet)).toMap
    }
    def squares(board: Map[(Int, Int), Int]): Map[(Int, Int), Set[Int]] = (for{
      x <- List(0, 3, 6)
      y <- List(0, 3, 6)
    } yield ((x, y), board.filter{
      case ((x2, y2), v) => v != 0 && x2 >= x && x2 < x + 3 && y2 >= y && y2 < y + 3
    }.map(_._2).toSet)).toMap

    def apply(suduku: String): Board = {
      val board = suduku.split('\n').zipWithIndex.flatMap{t1 => 
        t1._1.replace('.', '0').toList.zipWithIndex.map{t2 =>
          ((t2._2, t1._2), t2._1 - '0')
        }
      }.toMap
      new Board(board, hline(board), vline(board), squares(board))
    }
  }

  def solve(suduku: String) = {
    def solveR(board: Board, metMultiple: Boolean): Stream[Board] = {
      // println("solve board\n" + board.inspect)
      val unfilled = board.calculate
      // println(s"==========unfilled===========\n$unfilled\n")
      if(unfilled.isEmpty) Stream(board)
      else if(unfilled.exists(_._2.isEmpty)) {
        println("not a solution")
        Stream.empty // not a valid answer
      }else {
        if(metMultiple) {
          val ((x, y), vs) = unfilled.toList.sortBy(_._2.size).head
          vs.toStream.flatMap(v => solveR(board.fill(x, y, v), true))
        } else {  // reduce depth of tree, fill all correct number
          val (single, multiple) = unfilled.partition(_._2.size == 1)
          if(!single.isEmpty) solveR(board.fill(single.map(t => (t._1, t._2.head))), false)
          else {
            val ((x, y), vs) = multiple.toList.sortBy(_._2.size).head
            vs.toStream.flatMap(v => solveR(board.fill(x, y, v), true))
          }
        }
      }
    }
    solveR(Board(suduku), false)
  }

  def main(args: Array[String]): Unit = {
    val suduku = "..48...17\n67.9.....\n5.8.3...4\n3..74.1..\n.69...78.\n..1.69..5\n1...8.3.6\n.....6.91\n24...15.." ::
                 "9.46....5\n....8.42.\n5....2...\n4....6.92\n.5.4.38..\n1..2....7\n24..71.3.\n..3.2.9.1\n..5..9..4" ::
                 "..7...16.\n.41.....3\n...851...\n.1653..4.\n3....2.19\n49.......\n..34..25.\n.8...59..\n2.5.6...." ::
                 "..9..74..\n...9...6.\n6..4..3..\n24.....3.\n....21...\n.1..4.85.\n1.5.9...7\n..7..8..6\n...7..5.3" ::
                 ".3..9..5.\n....1...4\n5..8....7\n....3.2..\n6....9.4.\n..7..25..\n.....19..\n..69....8\n12.....7." :: Nil
    suduku.foreach(s => println(solve(s).head))
  }

}