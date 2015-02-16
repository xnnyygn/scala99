package in.xnnyygn.scala99.shared

import scala.language.higherKinds
import scala.collection.mutable

abstract class GraphBase[T, U] {
  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
  }
  case class Node(value: T) {
    var adj: List[Edge] = Nil
    // neighbors are all nodes adjacent to this node.
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil
  
  // If the edge E connects N to another node, returns the other node,
  // otherwise returns None.
  def edgeTarget(e: Edge, n: Node): Option[Node]

  override def equals(o: Any) = o match {
    case g: GraphBase[_,_] => (nodes.keys == g.nodes.keys &&
                               edges.map(_.toTuple) == g.edges.map(_.toTuple))
    case _ => false
  }
  def addNode(value: T) = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
    n
  }

  def toTermForm: (List[T], List[(T, T, U)]) = {
    (nodes.keys.toList, edges.map(_.toTuple))
  }
  def toAdjacentForm: List[(T, List[(T, U)])] = {
    nodes.toList.map{case (k1, n1) => (k1, n1.adj.map(e => (e.n2.value, e.value)))}
  }

  def findPaths(start: T, end: T): List[List[T]] = {
    def findPathsR(a: T, b: T, r: List[T]): List[List[T]] = {
      if(a == b) List(List(b))
      else nodes(a).neighbors.map(_.value).filterNot(r.contains(_)).flatMap{
        v => findPathsR(v, b, a :: r).map(a :: _)
      }
    }
    findPathsR(start, end, Nil)
  }
  
  def findCycles2(start: T): List[List[T]] = {
    def findCyclesR(a: T, r: List[T]): List[List[T]] = {
      // println(s"find cycles from $start current $a path $r")
      val na = nodes(a)
      na.adj.map(edgeTarget(_, na).get.value).flatMap{v =>
        // println(s"check node $v")
        if(v == start && r.length > 1) Some((start :: a :: r).reverse)
        else if(r.contains(v)) None
        else findCyclesR(v, a :: r)
      }

    }
    findCyclesR(start, Nil)
  }

  def findCycles(a: T): List[List[T]] = {
    val na = nodes(a)
    na.adj.map(edgeTarget(_, na).get.value).flatMap(b => findPaths(b, a).map(a :: _)).filter(_.length > 3)
  }

}

class Graph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Graph[_,_] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] = {
    // println(s"get target of edge $e of node $n")
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None
  }

  def addEdge(n1: T, n2: T, value: U) = {
    val e = new Edge(nodes(n1), nodes(n2), value)
    edges = e :: edges
    nodes(n1).adj = e :: nodes(n1).adj
    nodes(n2).adj = e :: nodes(n2).adj
  }

  override def toString: String = {
    (nodes.values.filter(_.adj.isEmpty).map(_.value).toList ::: 
      edges.map(e => e.n1.value + "-" + e.n2.value)).mkString("[", ", ", "]")
  }
}

class Digraph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Digraph[_,_] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else None

  def addArc(source: T, dest: T, value: U) = {
    val e = new Edge(nodes(source), nodes(dest), value)
    edges = e :: edges
    nodes(source).adj = e :: nodes(source).adj
  }

  override def toString: String = toAdjacentForm.flatMap{
    case (k1, adj) => {
      if(adj.isEmpty) k1.toString
      else adj.map {case (k2, v) => s"$k1>$k2/$v"}
    }
  }.mkString("[", ", ", "]")
}

abstract class GraphObjBase {
  type GraphClass[T, U]
  def addLabel[T](edges: List[(T, T)]) =
    edges.map(v => (v._1, v._2, ()))
  def term[T](nodes: List[T], edges: List[(T,T)]) =
    termLabel(nodes, addLabel(edges))
  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): GraphClass[T, U]
  def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
    nodes.map(a => (a._1, a._2.map((_, ()))))
  def adjacent[T](nodes: List[(T, List[T])]) =
    adjacentLabel(addAdjacentLabel(nodes))
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): GraphClass[T, U]
}

object Graph extends GraphObjBase {
  type GraphClass[T, U] = Graph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Graph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addEdge(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Graph[T, U]
    for ((v, a) <- nodes) g.addNode(v)
    for ((n1, a) <- nodes; (n2, l) <- a) {
      if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
        g.addEdge(n1, n2, l)
    }
    g
  }
  // [b-c, f-c, g-h, d, f-b, k-f, h-g]
  def fromString(s: String): Graph[String, Unit] = {
    val l = s.length
    if(l < 2 || (s(0) != '[' && s(l - 1) != ']')) throw new IllegalArgumentException("expect [n1-n2,*nk-nl]")
    val edges = mutable.ListBuffer[(String, String, Unit)]()
    val nodes = mutable.Set[String]()
    s.substring(1, l - 1).split(", ").foreach{p =>
      if(p.length == 1) nodes += p
      else {
        val n1 = p(0).toString
        val n2 = p(2).toString
        val edge = (n1, n2, ())
        edges += edge
        nodes += n1
        nodes += n2
      }
    }
    termLabel(nodes.toList, edges.toList)
  }
}

object Digraph extends GraphObjBase {
  type GraphClass[T, U] = Digraph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Digraph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addArc(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Digraph[T, U]
    for ((n, a) <- nodes) g.addNode(n)
    for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
    g
  }
  // [p>q/9, m>q/7, k, p>m/5]
  def fromStringLabel(s: String): Digraph[String, String] = {
    val l = s.length
    if(l < 2 || (s(0) != '[' && s(l - 1) != ']')) throw new IllegalArgumentException("expect [n1-n2,*nk-nl]")
    val edges = mutable.ListBuffer[(String, String, String)]()
    val nodes = mutable.Set[String]()
    s.substring(1, l - 1).split(", ").foreach{p =>
      if(p.length == 1) nodes += p
      else {
        val n1 = p(0).toString
        val n2 = p(2).toString
        val v = p(4).toString
        val edge = (n1, n2, v)
        edges += edge
        nodes += n1
        nodes += n2
      }
    }
    termLabel(nodes.toList, edges.toList)
  }
}