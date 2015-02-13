package in.xnnyygn.scala99.shared

sealed abstract class Tree[+T] {
  def mirrorTo[T](that: Tree[T]): Boolean
  def isSymmetric: Boolean
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
}

object Tree {
  def fromList[A <% Ordered[A]](xs: List[A]): Tree[A] = xs.foldLeft[Tree[A]](End)((acc, x) => acc.addValue(x))
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