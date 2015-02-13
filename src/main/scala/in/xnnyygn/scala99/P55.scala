package in.xnnyygn.scala99

import scala.collection.mutable

// a template for answer
object P55 {

  // method goes here
  object Tree {
    import shared._
    def cBalanced[A](n: Int, x: A): List[Tree[A]] = n match {
      case 1 => List(Node(x))
      case _ => removeDuplicated(cBalanced(n - 1, x).flatMap(addNode(_, Node(x))))
    }

    def removeDuplicated[A](ts: List[Tree[A]]): List[Tree[A]] = ts.toSet.toList

    def countLeftNodes(t: Tree[_]): Int = t match {
      case Node(_, l: Tree[_], _) => 1 + countLeftNodes(l)
      case _ => 0
    }

    def countRightNodes(t: Tree[_]): Int = t match {
      case Node(_, _, r: Tree[_]) => 1 + countRightNodes(r)
      case _ => 0
    }

    def addNode[A](t: Tree[A], x: Node[A]): List[Tree[A]] =  t match {
      case Node(v, End, End) => List(Node(v, x, End), Node(v, End, x))
      case Node(v, End, r) => List(Node(v, x, r))
      case Node(v, l, End) => List(Node(v, l, x))
      case Node(v, l: Tree[A], r: Tree[A]) => {
        val lc = countLeftNodes(l)
        val rc = countRightNodes(r)

        if(lc == rc) addNode(l, x).map(Node(v, _, r)) ::: addNode(r, x).map(Node(v, l, _))
        else if(lc < rc) addNode(l, x).map(Node(v, _, r))
        else addNode(r, x).map(Node(v, l, _))
      }
      case _ => throw new IllegalStateException("illegal state of adding node")
    }

    /* merge two subtree */
    def cBalanced2[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
      case n if n < 1 => List(End)
      case n if n % 2 == 1 => {
        val subtrees = cBalanced(n / 2, value)
        subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
      }
      case n if n % 2 == 0 => {
        val lesserSubtrees = cBalanced((n - 1) / 2, value)
        val greaterSubtrees = cBalanced((n - 1) / 2 + 1, value)
        lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
      }
    }

    // private val treeCache = mutable.Map[(Int, Any), List[Tree[Any]]](0 -> List(End))

    def generateBalancedTree[A](n: Int, x: A): List[Tree[A]] = {
      // println(s"generate balance tree $n")
      def generateBalancedTreeR(m: Int): List[Tree[A]] = {
        if(m == 0) List(End)
        else if((m & 1) == 1) generateBalancedTreeR(m >> 1).map(t => Node(x, t, t))
        else (for{
          t1 <- generateBalancedTreeR((m - 1) >> 1)
          t2 <- generateBalancedTreeR((m + 1) >> 1)
        } yield List(Node(x, t1, t2), Node(x, t2, t1))).flatten
      }
      generateBalancedTreeR(n)
    }
  }

  def main(args: Array[String]): Unit = {
    println(Tree.generateBalancedTree(4, "x"))
    println(Tree.generateBalancedTree(4, "y"))
    // println(Tree.cBalanced2(4, "x"))
  }

}