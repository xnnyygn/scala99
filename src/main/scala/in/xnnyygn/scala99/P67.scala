package in.xnnyygn.scala99

// a template for answer
object P67 {

  // method goes here
  sealed abstract class Tree[+A] {
    def inspect: String
  }
  case class Node[+A](x: A, left: Tree[A] = End, right: Tree[A] = End) extends Tree[A] {
    def inspect: String = {
      val l = left.inspect
      val r = right.inspect
      s"T($x $l $r)"
    }
    override def toString = {
      if(left == End && right == End) x.toString
      else s"$x($left,$right)"
    }
  }
  case object End extends Tree[Nothing] {
    def inspect: String = "."
    override def toString = ""
  }
  object Tree {
    // "a(b(d,e),c(,f(g,)))"
    def fromString(s: String): Tree[String] = {
      def parseConstantChar(cs: List[Char], c: Char): (Char, List[Char]) = cs match {
        case `c` :: tail => (c, tail)
        case _ => throw new IllegalArgumentException(s"expect char $c")
      }
      def parseOptionalChar(cs: List[Char], c: Char): (Option[Char], List[Char]) = cs match {
        case `c` :: tail => (Some(c), tail)
        case _ => (None, cs)
      }
      def parseChars(cs: List[Char], ds: Set[Char]): (List[Char], List[Char]) = cs match {
        case Nil => (Nil, Nil)
        case c :: _ if ds.contains(c) => (Nil, cs)
        case c :: tail => {
          val (s, cs) = parseChars(tail, ds)
          (c :: s, cs)
        }
      }
      // 1 def tree = end | node
      // 2 def end  = <emptry string>
      // 3 def node = value | value ( tree , tree )
      def parseTree(cs: List[Char]): (Tree[String], List[Char]) = {
        if(cs.isEmpty) (End, Nil)
        else {
          val (v, cs1) = parseChars(cs, Set('(', ',', ')'))
          val value = v.mkString
          // println(s"value $value")
          parseOptionalChar(cs1, '(') match {
            case (None, cs2) => (Node(value), cs2) // rule 3.1
            case (_, cs2) => { // rule 3.2
              // val (_, cs3) = parseConstantChar(cs2, '(')
              val (left, cs4) = parseTree(cs2)
              val (_, cs5) = parseConstantChar(cs4, ',')
              val (right, cs6) = parseTree(cs5)
              // println(cs6.mkString(" "))
              val (_, cs7) = parseConstantChar(cs6, ')')
              (Node(value, left, right), cs7)
            }
          }
        }
      }
      /* def parseNode(cs: List[Char]): (Tree[Char], List[Char]) = cs match {
        case Nil => (End, Nil)
        case c :: tail if c == ',' || c == ')' => (End, cs)
        case v :: Nil => (Node(v), Nil)
        case v :: c :: tail if c == ',' || c == ')'  => (Node(v), c :: tail)
        case v :: '(' :: tail => {
          val (left, r1) = parseNode(tail)
          val r2 = parseConstantChar(r1, ',')
          val (right, r3) = parseNode(r2)
          val r4 = parseConstantChar(r3, ')')
          (Node(v, left, right), r4)
        }
        case _ => throw new IllegalArgumentException("expect , ( or EOF")
      } */
      parseTree(s.toList)._1
    }
  }

  /* def main(args: Array[String]): Unit = {
    val s = "a(b(d,e),c(,f(g,)))"
    println(s)
    println(Tree.fromString(s).inspect)
  } */
}