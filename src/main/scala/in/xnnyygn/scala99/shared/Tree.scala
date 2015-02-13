package in.xnnyygn.scala99.shared

sealed abstract class Tree[+T] {
  override def hashCode: Int = toString.hashCode
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = s"T($value $left $right)"
}

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}