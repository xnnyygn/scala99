package in.xnnyygn.scala99.shared

import scala.math
import in.xnnyygn.scala99.shared.Math.pow2

sealed abstract class Tree[+T] {
  def mirrorTo[T](that: Tree[T]): Boolean
  def isSymmetric: Boolean
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
  def nodeCounts: Int
  def leafCount: Int
  def leafList: List[T]
  def internalList: List[T]
  def atLevel(d: Int): List[T]

  def layoutBinaryTree(x: Int, y: Int): (Tree[T], Int)
  def layoutBinaryTree2(x: Int, y: Int, h: Int): Tree[T]
  def height: Int
  def leftMostDepth: Int
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

  def completeBinaryTree[A](n: Int, x: A): Tree[A] = {
    def createNode(parentId: Int): Node[A] = {
      val leftId = parentId * 2 + 1
      val rightId = parentId * 2 + 2
      val left = if(leftId < n) createNode(leftId) else End
      val right = if(rightId < n) createNode(rightId) else End
      Node(x, left, right)
    }
    createNode(0)
  }

  def completeBinaryTree2[T](nodes: Int, value: T): Tree[T] = {
    def generateTree(addr: Int): Tree[T] =
      if (addr > nodes) End
      else Node(value, generateTree(2 * addr), generateTree(2 * addr + 1))
    generateTree(1)
  }

  /* def main(args: Array[String]): Unit = {
    val t = Tree.fromList(List('n','k','m','c','a','e','d','g','u','p','q'))
    println(t)
    println(t.asInstanceOf[Node[Symbol]].layoutBinaryTree2)
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
  def leafCount: Int = {
    if(isLeaf) 1
    else left.leafCount + right.leafCount
  }
  private def isLeaf: Boolean = (left == End && right == End)
  def leafList: List[T] = {
    if(isLeaf) List(value)
    else left.leafList ::: right.leafList
  }
  def internalList: List[T] = {
    if(isLeaf) Nil
    else value :: left.internalList ::: right.internalList
  }
  def atLevel(d: Int): List[T] = {
    if(d == 1) List(value)
    else left.atLevel(d - 1) ::: right.atLevel(d - 1)
  }

  def layoutBinaryTree: Tree[T] = layoutBinaryTree(1, 1)._1
  def layoutBinaryTree(x: Int, y: Int): (Tree[T], Int) = {
    val (left2, x2) = left.layoutBinaryTree(x, y + 1)
    val (right2, x3) = right.layoutBinaryTree(x2 + 1, y + 1)
    (new PositionedNode(value, left2, right2, x2, y), x3)
  }
  def height: Int = 1 + math.max(left.height, right.height)
  def leftMostDepth: Int = 1 + left.leftMostDepth
  def layoutBinaryTree2: Tree[T] = {
    val h = height
    /*
    delta(y) = if(h == y) 0 else pow2(h - y - 1)
    offset(lm) = delta(1) + ... + delta(lm - 1) + 1
               = 2^(h - lm) + ... + 2^(h - 1) + 1
               = 2^(h - 1) - 2^(h - lm) + 1
    */
    val xOffset = pow2(h - 1) - pow2(h - leftMostDepth) + 1
    layoutBinaryTree2(xOffset, 1, h)
  }
  def layoutBinaryTree2(x: Int, y: Int, h: Int): Tree[T] = {
    // println(s"layout $value $x $y $h")
    val delta = if(h == y) 0 else pow2(h - y - 1)
    new PositionedNode(value, 
      left.layoutBinaryTree2(x - delta, y + 1, h), 
      right.layoutBinaryTree2(x + delta, y + 1, h), x, y)
  }
  /* def layoutBinaryTree2b(x: Int, y: Int, h: Int): (Tree[T], Int) = {
    val delta = if(h == y) 0 else pow2(h - y - 1)
    // println(s"layout $value $x $y $h delta $delta")
    val (left2, x2) = left.layoutBinaryTree2b(x, y + 1, h)
    val (right2, x3) = right.layoutBinaryTree2b(x2 + delta, y + 1, h)
    (new PositionedNode(value, left2, right2, x2, y), x2 + 2 * delta)
  } */
  override def toString = s"T($value $left $right)"
}

case object End extends Tree[Nothing] {
  def mirrorTo[T](that: Tree[T]): Boolean = that == End
  def isSymmetric: Boolean = true
  def nodeCounts: Int = 0
  def addValue[U <% Ordered[U]](x: U): Tree[U] = Node(x, End, End)
  def leafCount: Int = 0
  def leafList   = Nil
  def internalList = Nil
  def atLevel(d: Int) = Nil
  def layoutBinaryTree(x: Int, y: Int) = (End, x)
  def layoutBinaryTree2(x: Int, y: Int, h: Int): Tree[Nothing] = End
  def height: Int = 0
  def leftMostDepth: Int = 0
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
  def getX = x
  override def toString = s"T[$x,$y]($value $left $right)"
}