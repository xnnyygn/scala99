package in.xnnyygn.scala99.shared

import scala.language.implicitConversions

case class MTree[+A](value: A, children: List[MTree[A]] = Nil) {
  def nodeCount: Int = children.foldLeft(1)(_ + _.nodeCount)
  def internalPathLength: Int = internalPathLength(0)
  /* def internalPathLength: Int = 
    children.foldLeft(0)((r, c) => r + c.nodeCount + c.internalPathLength) */
  // calculate by depth
  def internalPathLength(d: Int): Int = d + children.foldLeft(0)(_ + _.internalPathLength(d + 1))

  // override def toString = "M(" + value + " {" + children.mkString(",") + "})"
  override def toString: String = value.toString + children.mkString + "^"
}

object MTree {

  // BNF
  // s     = mtree
  // mtree = value mtree* ^
  // value = !^
  /* def string2MTree(s: String): MTree[Char] = {
    def parseConstantChar(p: Int, c: Char): (Char, Int) = s(p) match {
      case `c` => (c, p + 1)
      case _ => throw new IllegalArgumentException(s"expect $c")
    }
    def parseValue(p: Int): (Char, Int) = s(p) match {
      case '^' => throw new IllegalArgumentException("expect value")
      case c => (c, p + 1)
    }
    def parseOptionalValue(p: Int): (Option[Char], Int) = s(p) match {
      case '^' => (None, p)
      case value => (Some(value), p + 1)
    }
    def parseOptionalMTree(p: Int): (Option[MTree[Char]], Int) = parseOptionalValue(p) match {
      case (None, p2) => (None, p2)
      case (Some(value), p2) => {
        val (children, p3) = parseMTrees(p2)
        val (_, p4) = parseConstantChar(p3, '^')
        (Some(MTree(value, children)), p4)
      }
    }
    def parseMTrees(p: Int): (List[MTree[Char]], Int) = parseOptionalMTree(p) match {
      case (None, p2) => (Nil, p2)
      case (Some(m), p2) => {
        val (ms, p3) = parseMTrees(p2)
        (m :: ms, p3)
      }
    }
    def parseMTree(p: Int): (MTree[Char], Int) = {
      val (value, p2) = parseValue(p)
      val (children, p3) = parseMTrees(p2)
      val (_, p4) = parseConstantChar(p3, '^')
      (MTree(value, children), p4)
    }
    parseMTree(0)._1
  } */

  implicit def string2MTree(s: String): MTree[Char] = {
    import SyntaxRule._

    def value = liternal(_ != '^', "value")
    def mtree: SyntaxRule[MTree[Char]] = (
      value + repeat(mtree) + liternal('^')
    ).map{case ((v, children), _) => MTree(v, children)}

    mtree(s, 0)._1
  }

  /* def main(args: Array[String]): Unit = {
    println(string2MTree("afg^^c^bd^e^^^"))
  } */
  
}