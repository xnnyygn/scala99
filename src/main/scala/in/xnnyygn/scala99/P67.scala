package in.xnnyygn.scala99

// a template for answer
object P67 {

  // method goes here
  sealed abstract class Tree[+A] {
    def inspect: String
    def preorder: List[A]
    def inorder: List[A]
  }
  case class Node[+A](x: A, left: Tree[A] = End, right: Tree[A] = End) extends Tree[A] {
    def inspect: String = {
      val l = left.inspect
      val r = right.inspect
      s"T($x $l $r)"
    }
    def preorder: List[A] =  x :: left.preorder ::: right.preorder
    def inorder: List[A] = left.inorder ::: x :: right.inorder
    override def toString = {
      if(left == End && right == End) x.toString
      else s"$x($left,$right)"
    }
  }
  case object End extends Tree[Nothing] {
    def inspect: String = "."
    def preorder = Nil
    def inorder = Nil
    override def toString = ""
  }

  object Tree {
    def preInTree[A](preorder: List[A], inorder: List[A]): Tree[A] = {
      def splitB(x: A, inorder2: List[A]): (List[A], List[A]) = inorder2 match {
        case Nil => (Nil, Nil)
        case `x` :: tail => (Nil, tail)
        case h :: tail => {
          val (l, r) = splitB(x, tail)
          (h :: l, r)
        }
      }
      def splitA(lvs: Set[A], preorder2: List[A]): (List[A], List[A]) = preorder2 match {
        case Nil => (Nil, Nil)
        case h :: tail if !lvs.contains(h) => (Nil, preorder2)
        case h :: tail => {
          val (l, r) = splitA(lvs, tail)
          (h :: l, r)
        }
      }
      (preorder, inorder) match {
        case (Nil, Nil) => End
        case (a :: Nil, b :: Nil) if a == b => Node(a)
        case _ => {
          val x :: alr = preorder
          val (bl, br) = splitB(x, inorder)
          val (al, ar) = splitA(bl.toSet, alr)
          // println(s"$al $ar")
          // println(s"$bl $br")
          Node(x, preInTree(al, bl), preInTree(ar, br))
        }
      }
    }
    // "a(b(d,e),c(,f(g,)))"
    def fromString(s: String): Tree[Char] = {
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
      def parseTree(cs: List[Char]): (Tree[Char], List[Char]) = {
        if(cs.isEmpty) return (End, Nil)
        val (v, cs1) = parseChars(cs, Set('(', ',', ')'))
        if(v.isEmpty) return (End, cs1)
        val value = v.head // from string to char
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

      // for string version
      def parseTree2(cs: List[Char]): (Tree[String], List[Char]) = {
        if(cs.isEmpty) return (End, Nil)
        val (v, cs1) = parseChars(cs, Set('(', ',', ')'))
        if(v.isEmpty) return (End, cs1)
        val value = v.mkString
        // println(s"value $value")
        parseOptionalChar(cs1, '(') match {
          case (None, cs2) => (Node(value), cs2) // rule 3.1
          case (_, cs2) => { // rule 3.2
            // val (_, cs3) = parseConstantChar(cs2, '(')
            val (left, cs4) = parseTree2(cs2)
            val (_, cs5) = parseConstantChar(cs4, ',')
            val (right, cs6) = parseTree2(cs5)
            // println(cs6.mkString(" "))
            val (_, cs7) = parseConstantChar(cs6, ')')
            (Node(value, left, right), cs7)
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
    val t = Tree.fromString(s)
    println(t.inspect)
    println(t.preorder)
    println(t.inorder)
  } */
}