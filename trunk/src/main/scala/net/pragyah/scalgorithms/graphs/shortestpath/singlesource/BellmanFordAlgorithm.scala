package net.pragyah.scalgorithms.graphs.shortestpath.singlesource


import net.pragyah.scalgorithms.trees.Tree

/*Bellman-Ford Algorithm solves single-source shortest-paths problem in general case
 * in which edge weights may be negative
 * It either throws an exception saying that a negative weight cycle exists
 * or returns back  shortest path tree
 * Running time - O(V.E)
 */

class BellmanFordAlgorithm[A] extends ShortestPathAlgo[A] {
  
  type tagClass = Tuple2[double,Option[Vertex[A]]]  // value,parent
  
  implicit def vertexToDouble(v:Vertex[A]) : double = v.tag.asInstanceOf[tagClass]._1

  def getShortestPath(graph:Graph[A], s:Vertex[A]): Tree[Vertex[A]] = {
    graph.vertices.foreach(_.tag = (Double.MaxValue,None)) // THETA(V) time
    s.tag = (0d,None)
  
    /* relax all the edges and repeate this process n-1 times
     * By n-1 cycles the shortest-path weights should converge
     * unless there is a negavite weight cycles
     * 
     * total time O(VE)
     */
    
    for(i <- 1 to (graph.vertices.length) -1) {
      
      //THEAT(E) time
      graph.edges.foreach(e => 
                            if(e.v2 > e.v1 + e.weight)//implicit conversion to double .. just trying out implicit in scala :) 
                              e.v2.tag = (e.v1 + e.weight,Some(e.v1))
                          )
    }

    // O(E)
    assume(graph.edges.filter( e => e.v2 > e.v1 + e.weight).isEmpty, "There is a negative weight cycle")

    TreeGenerator.generateTrees(graph,getVertexPredecessor _).head
//    null; // TODO
  }
  
  def getVertexPathWeight(v:Vertex[A]) : double = v
  def getVertexPredecessor(v:Vertex[A]) : Option[Vertex[A]] = v.tag.asInstanceOf[tagClass]._2
  
}
