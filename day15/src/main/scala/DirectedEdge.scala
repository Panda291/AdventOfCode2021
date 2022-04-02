final case class DirectedEdge(from: (Int, Int), to: (Int, Int), weight: Double)

final case class EdgeWeightedDigraph(adj: Map[(Int, Int), List[DirectedEdge]] = Map.empty)

object EdgeWeightedDigraphOps {

  implicit class EdgeWeightedDigraphOps(g: EdgeWeightedDigraph) {
    /**
     * Adds directed edge 'e' to EdgeWeightedDigraph by creating
     * a full-copy adjacency list, i.e. this operation
     * leaves current graph 'g' immutable
     *
     * @param e - DirectedEdge to add into the graph 'g'
     * @return returns new graph with edge 'e' added to it
     */
    def addEdge(e: DirectedEdge): EdgeWeightedDigraph = {
      val list = g.adj.getOrElse(e.from, List.empty)
      val adj = g.adj + (e.from -> (list :+ e))
      EdgeWeightedDigraph(adj)
    }
  }

}