package in.xnnyygn.scala99.shared

sealed abstract class Tree[+T] {
  def mirrorTo[T](that: Tree[T]): Boolean
  def isSymmetric: Boolean
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def mirrorTo[T](that: Tree[T]): Boolean = that match {
    case Node(_, left2, right2) => left.mirrorTo(right2) && right.mirrorTo(left2)
    case _ => false
  }
  def isSymmetric: Boolean = left.mirrorTo(right)
  override def toString = s"T($value $left $right)"
}

case object End extends Tree[Nothing] {
  def mirrorTo[T](that: Tree[T]): Boolean = that == End
  def isSymmetric: Boolean = true
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}