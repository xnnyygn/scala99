package in.xnnyygn.scala99.shared

sealed abstract class Tree[+T] {
  def mirrorTo[T](that: Tree[T]): Boolean
  def isSymmetric: Boolean
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
}

object Tree {
  def fromList[A <% Ordered[A]](xs: List[A]): Tree[A] = xs.foldLeft[Tree[A]](End)((acc, x) => acc.addValue(x))

  def generateBalancedTree[A](n: Int, x: A): List[Tree[A]] = {
    if(n == 0) List(End)
    else if((n & 1) == 1) {
      val subTrees = generateBalancedTree(n >> 1, x)
      subTrees.flatMap(l => subTrees.map(r => Node(x, l, r)))
    } else (for{
      t1 <- generateBalancedTree((n - 1) >> 1, x)
      t2 <- generateBalancedTree((n + 1) >> 1, x)
    } yield List(Node(x, t1, t2), Node(x, t2, t1))).flatten
  }

  def symmetricBalancedTrees[A](n: Int, x: A): List[Tree[A]] = generateBalancedTree(n, x).filter(_.isSymmetric)
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def mirrorTo[T](that: Tree[T]): Boolean = that match {
    case Node(_, left2, right2) => left.mirrorTo(right2) && right.mirrorTo(left2)
    case _ => false
  }
  def isSymmetric: Boolean = left.mirrorTo(right)
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = {
    if(x < value) Node(value, left.addValue(x), right)
    else Node(value, left, right.addValue(x))
  }
  override def toString = s"T($value $left $right)"
}

case object End extends Tree[Nothing] {
  def mirrorTo[T](that: Tree[T]): Boolean = that == End
  def isSymmetric: Boolean = true
  def addValue[U <% Ordered[U]](x: U): Tree[U] = Node(x, End, End)
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}