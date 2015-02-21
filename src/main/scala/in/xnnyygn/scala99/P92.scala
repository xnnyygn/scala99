package in.xnnyygn.scala99

import scala.collection.mutable

// a template for answer
object P92 {

  type VertexName = Char

  case class Vertex(name: VertexName, adjacent: Set[VertexName] = Set()) {
    def degree: Int = adjacent.size

    override def hashCode = name.hashCode
    override def equals(o: Any) = o match {
      case v: Vertex => v.name == this.name
      case _ => false
    }
    override def toString = s"[$name, $adjacent]"
  }

  // tree format: edge1-edge2*, e.g: a-b c-d
  def generateVertexAdjacentTable(tree: String): Map[VertexName, Vertex] = {
    def addAdjacentToVertex(m: Map[VertexName, Set[VertexName]], 
      n: VertexName, adj: VertexName): (VertexName, Set[VertexName]) = m.get(n) match {
      case Some(adjacent) => n -> (adjacent + adj)
      case _ => n -> Set(adj)
    }

    tree.split(' ').foldLeft(Map[VertexName, Set[VertexName]]()){(m, p) => 
      m + addAdjacentToVertex(m, p(0), p(2)) + addAdjacentToVertex(m, p(2), p(0))
    }.map(t => (t._1, Vertex(t._1, t._2)))
  }

  case class Edge(v1: VertexName, v2: VertexName)

  def vonKoch(tree: String): Iterator[Map[VertexName, Int]] = {
    val edges = tree.split(' ').map(p => Edge(p(0), p(2))).toSet
    val vertexes = edges.flatMap(e => List(e.v1, e.v2)).toList
    val n = vertexes.size
    List.range(1, n + 1).permutations.map(vertexes.zip(_).toMap).filter{mapping =>
      val elabels = edges.map(e => scala.math.abs(mapping(e.v1) - mapping(e.v2)))
      // println(s"test mapping $mapping elables $elabels")
      elabels.size == n - 1 && elabels.forall(x => x >= 1 && x < n)
    }
  }

  // incorrect
  def gracefulLabelIncorrect(tree: String): Map[VertexName, Int] = {

    val vertexes = generateVertexAdjacentTable(tree)

    def gracefulLabelR(vertexCandidates: List[VertexName], mapping: Map[VertexName, Int], 
      vertexLabelCandidates: List[Int], edgeLabelCandidates: List[Int]): Either[Unit, Map[VertexName, Int]] = {
      println(s"vertexes $vertexCandidates mapping $mapping vertex labels $vertexLabelCandidates edge labels $edgeLabelCandidates")
      if(vertexCandidates.isEmpty) Right(mapping)
      else {
        val v :: remainingVertexes = vertexCandidates
        if(!mapping.contains(v)) {
          val label :: others = vertexLabelCandidates
          // TODO change vertex label candidates to set
          gracefulLabelR(vertexCandidates, mapping + (v -> label), others, edgeLabelCandidates)
        } else {
          val unsetAdjacent = vertexes(v).adjacent.filterNot(mapping.contains)
          val vertexLabel = mapping(v)
          val (edgeLabels, remainingEdgeLabels) = edgeLabelCandidates.splitAt(unsetAdjacent.size)
          val adjacentLabels = edgeLabels.map{edgeLabel =>
            val label = vertexLabel - edgeLabel
            if(label > 0 && vertexLabelCandidates.contains(label)) label
            else vertexLabel + edgeLabel
          }
          // println(s"test vertex labels $vertexLabelCandidates $adjacentLabels")
          val remainingVertexLabels = vertexLabelCandidates.filterNot(adjacentLabels.contains)
          if(remainingVertexLabels.size != vertexLabelCandidates.size - adjacentLabels.size) Left()
          else {
            for(adjacent <- unsetAdjacent.toList.permutations) {
              gracefulLabelR(remainingVertexes, mapping ++ adjacent.zip(adjacentLabels),
                remainingVertexLabels, remainingEdgeLabels) match {
                case r : Right[_, _] => return r
                case _ => ()
              }
            }
            Left()
          }
        }
      }
    }    

    val vertexCandidates = vertexes.values.toList.sortBy(-_.degree).map(_.name)
    val vertexCount = vertexes.size
    gracefulLabelR(vertexCandidates, Map(), 
      List.range(vertexCount, 0, -1), List.range(vertexCount - 1, 0, -1)) match {
      case Right(mapping) => mapping
      case _ => throw new IllegalStateException("failed to found mapping")
    }
  }

  /* def main(args: Array[String]): Unit = {
    // val tree = "a-d a-g a-b b-e b-c e-f"
    val tree = "a-i a-h a-b a-g a-c c-d c-f c-e d-k e-q q-m q-n p-n"
    println(vonKoch(tree).next)
  } */
}