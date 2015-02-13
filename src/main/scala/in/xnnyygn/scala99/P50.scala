package in.xnnyygn.scala99

import scala.reflect.ClassTag

// a template for answer
object P50 {

  type Code = List[Int]

  // method goes here
  def huffman(ks: List[(String, Int)]): List[(String, String)] = {
    // not check wheter list length = 1
    val mh = MinHeap[AbstractNode](ks.map{case (k, f) => Leaf(k, f)})
    // otherwise scala builtin priority queue
    /* val mh = scala.collection.mutable.PriorityQueue[AbstractNode](
      ks.map{case (k, f) => Leaf(k, f)}: _*)(
      Ordering.by[AbstractNode, Int](n => -n.getFrequency)) */
    // println(mh)

    def decreaseNode(): Node = {
      val n = mh.size
      if(n <= 0) throw new IllegalStateException("illegal state of min heap")
      else if(n == 1) mh.dequeue.asInstanceOf[Node]
      else {
        val a = mh.dequeue
        val b = mh.dequeue
        val ab = Node(a, a.getFrequency + b.getFrequency, b)
        // println(s"insert node $ab")
        mh.enqueue(ab)
        // println(mh)
        decreaseNode()
      }
    }

    val huffmanTree = decreaseNode()
    val huffmanDict = encode(huffmanTree)
    // println(huffmanTree)
    // println(huffmanDict)
    ks.map{case (k, _) => (k, huffmanDict(k).mkString)}
  }

  object MinHeap {
    def apply[A <% Ordered[A]](xs: Traversable[A])(implicit ct: ClassTag[A]): MinHeap[A] = {
      val mh = new MinHeap(xs.toArray)
      mh.init
      mh
    }
  }

  class MinHeap[A <% Ordered[A]](private val array: Array[A]) {
    private var length = array.length
    def size: Int = length

    def enqueue(x: A): Unit = insert(x)
    def dequeue: A = deleteMin

    /**
     * Delete minimum element.
     * If empty, throw IllegalStateException.
     * Swap last element and root element and shift down.
     */
    def deleteMin: A = {
      if(length == 0) throw new IllegalStateException("heap is empty")
      val x = array(0)
      swap(0, length - 1) // swap last element and root element
      length -= 1
      shiftDown(0)
      x
    }

    /**
     * Insert element into heap.
     * No ensure capacity.
     * Add element to last position and shift up element.
     */
    def insert(x: A): Unit = {
      array.update(length, x)
      shiftUp(length)
      length += 1
    }

    /**
     * Arrange elements to meet heap.
     * From Ln/2 downTo 0
     */
    /* private[MinHeap] */def init: Unit = for(i <- array.length / 2 to 0 by -1) shiftDown(i)

    /**
     * Swap elements in array.
     * Not check range.
     * Not check i and j equality.
     */
    private def swap(i: Int, j: Int) : Unit = {
      /* val ai = array(i)
      val aj = array(j)
      println(s"swap $ai $aj")*/
      val t = array(i)
      array.update(i, array(j))
      array.update(j, t)
    }

    /**
     * Shift down element.
     * Sense: add new element, from bottom.
     *
     *   a
     *  / \
     * b   c
     * 
     * shiftUp(b): if b > a, swap a, b then shiftUp(b)
     */
    private def shiftUp(i: Int): Unit = {
      if(i == 0) return // ignore root
      val parent = i / 2
      if(array(i) < array(parent)) {
        swap(parent, i)
        shiftUp(parent)
      }
    }

    /**
     * Shift down element.
     * Sense: 
     * 1. delete root, from top.
     * 2. make heap
     */
    private def shiftDown(i: Int): Unit = {
      // println(s"shift down $i")
      val left = i * 2 + 1
      if(left >= length) return // ignore leaf
      var min = left
      val right = left + 1
      if(right < length && array(right) < array(left)) min = right
      if(array(min) < array(i)) {
        swap(min, i)
        shiftDown(min)
      }
    }
    override def toString: String = array.take(length).mkString(", ")
  }

  sealed abstract class AbstractNode(frequency: Int) extends Ordered[AbstractNode] {
    def getFrequency: Int = frequency
    def compare(that: AbstractNode): Int = frequency - that.getFrequency
  }
  case class Node(left: AbstractNode, frequency: Int, right: AbstractNode) extends AbstractNode(frequency)
  case class Leaf(key: String, frequency: Int) extends AbstractNode(frequency)

  def encode(node: AbstractNode): Map[String, Code] = {
    def encodeR(node: AbstractNode, path: Code): Map[String, Code] = node match {
      case Leaf(key, _) => Map(key -> path.reverse)
      case Node(left, _, right) => encodeR(left, 0 :: path) ++ encodeR(right, 1 :: path)
    }
    encodeR(node, Nil)
  }

  /* def main(args: Array[String]): Unit = {
    val data = List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))
    println(data)
    val q = scala.collection.mutable.PriorityQueue[AbstractNode](
      data.map{case (k, f) => Leaf(k, f)}: _*
    )(Ordering.by[AbstractNode, Int](n => -n.getFrequency))
    println(q.dequeueAll)
  } */

}