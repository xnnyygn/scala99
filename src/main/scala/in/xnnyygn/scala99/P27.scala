package in.xnnyygn.scala99

// a template for answer
object P27 {

  // method goes here
  // group to 2, 3, 4
  // take 2 from (1...9)
  // (a, b) take 3 from (1...9 - (a, b))
  // (a, b, c, d, e) take 4 from (remaining)
  def group3c[A](xs: List[A]): List[List[List[A]]] = {
    // List[(remaining, x, availables)]
    def split[A](k: Int, ys: List[A]): List[(List[A], A, List[A])] = {
      // println(s"split $k $ys")
      if(k < 0 || k > ys.length || ys.isEmpty) Nil
      List.range(0, k).map{i =>
        ys.splitAt(i) match {
          case (_, Nil) => throw new IllegalStateException("unexpected error")
          case (pre, h :: post) => (pre, h, post)
        }
      }
    }
    // List[(elements, remaining)]
    def takeN[A](n: Int, ys: List[A]): List[(List[A], List[A])] = {
      // println(s"take $n from $ys")
      if(n < 0) throw new IllegalStateException("n < 0")
      else if(n == 0) Nil
      else if(n == 1) split(ys.length, ys).map {
        case (rn, x, availables) => (List(x), rn ::: availables)
      }
      else split(ys.length - n + 1, ys).flatMap {
        case (rn, x, availables) => takeN(n - 1, availables).map {
          case (elements, rn1) => (x :: elements, rn ::: rn1)
        }
      }
    }
    for{
      (g1, r1) <- takeN(2, xs)
      (g2, g3) <- takeN(3, r1)
    } yield List(g1, g2, g3)
  }

  import P26.combinations

  def group3b[A](ls: List[A]): List[List[List[A]]] =
    for {
      a <- combinations(2, ls)
      noA = (ls.toSet -- a.toSet).toList
      b <- combinations(3, noA)
    } yield List(a, b, (noA.toSet -- b.toSet).toList)

  def group3[A](xs: List[A]): List[List[List[A]]] = {
    val set = xs.toSet
    for {
      g1 <- combinations(2, xs)
      r1 = (set -- g1).toList
      g2 <- combinations(3, r1)
    } yield List(g1, g2, (r1.toSet -- g2).toList)
  }

  def group[A](ns: List[Int], xs: List[A]): List[List[List[A]]] = ns match {
    case Nil => List(Nil)
    case h :: tail => combinations(h, xs).flatMap {g =>
      group(tail, (xs.toSet -- g).toList).map( g :: _)
    }
  }

}