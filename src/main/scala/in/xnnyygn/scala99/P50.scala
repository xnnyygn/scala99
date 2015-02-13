package in.xnnyygn.scala99

// a template for answer
object P50 {

  type Code = List[Int]

  // method goes here
  // def huffman(ks: List[(String, Int)]): List[(String, Code)] = Nil
  object MinHeap {
    def apply[A](xs: Traversable[A]): MinHeap[A] = throw new UnsupportedOperationException
  }
  abstract class MinHeap[A](private val array: Array[A], private var _length: Int) {
    def length: Int = _length
    def deleteMin: A
    def insert(x: A): Unit
  }

  sealed abstract class AbstractNode
  case class Node(left: AbstractNode, frequency: Int, right: AbstractNode) extends AbstractNode
  case class Leaf(key: String, frequency: Int) extends AbstractNode

  def encode(node: AbstractNode): List[(String, Code)] = {
    def encodeR(node: AbstractNode, path: Code): List[(String, Code)] = node match {
      case Leaf(key, _) => List((key, path.reverse))
      case Node(left, _, right) => encodeR(left, 0 :: path) ::: encodeR(right, 1 :: path)
    }
    encodeR(node, Nil)
  }

}