package in.xnnyygn.scala99.shared

sealed abstract class Tree[+T] {
  def mirrorTo[T](that: Tree[T]): Boolean
  def isSymmetric: Boolean
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
  def nodeCounts: Int
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

  private def pow2(n: Int): Int = {
    if(n == 0) 1
    else pow2(n - 1) << 1
  }

  def hbalTrees2[T](height: Int, value: T): List[Tree[T]] = height match {
    case n if n < 1 => List(End)
    case 1          => List(Node(value))
    case _ => {
      val fullHeight = hbalTrees2(height - 1, value)
      val short = hbalTrees2(height - 2, value)
      fullHeight.flatMap((l) => fullHeight.map((r) => Node(value, l, r))) :::
      fullHeight.flatMap((f) => short.flatMap((s) => List(Node(value, f, s), Node(value, s, f))))
    }
  }

  def hbalTrees3[A](h: Int, x: A): List[Tree[A]] = {
    if(h < 1) List(End)
    else if(h == 1) List(Node(x))
    else {
      val full = hbalTrees3(h - 1, x)
      val less = hbalTrees3(h - 2, x)
      full.flatMap(l => full.map(r => Node(x, l, r))) :::
        full.flatMap(l => less.flatMap(r => List(Node(x, l, r), Node(x, r, l))))
    }
  }

  def minHbalNodes(h: Int): Int = h match {
    case 0 => 0
    case 1 => 1
    case _ => minHbalNodes(h - 1) + minHbalNodes(h - 2) + 1
  }
  def maxHbalNodes(h: Int): Int = pow2(h) - 1
  def minHbalHeight(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => 1 + minHbalHeight(n >> 1)
  }
  def maxHbalHeight(n: Int): Int = Stream.from(1).takeWhile(h => minHbalNodes(h) <= n).last
  def hbalTreesWithNodes[A](n: Int, x: A): List[Tree[A]] = {
    List.range(minHbalHeight(n), maxHbalHeight(n) + 1).flatMap(hbalTrees3(_, x).filter(_.nodeCounts == n))
  }

  /* def main(args: Array[String]): Unit = {
    println(hbalTreesWithNodes(4, "x"))
  } */
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
  def nodeCounts: Int = 1 + left.nodeCounts + right.nodeCounts
  override def toString = s"T($value $left $right)"
}

case object End extends Tree[Nothing] {
  def mirrorTo[T](that: Tree[T]): Boolean = that == End
  def isSymmetric: Boolean = true
  def nodeCounts: Int = 0
  def addValue[U <% Ordered[U]](x: U): Tree[U] = Node(x, End, End)
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}