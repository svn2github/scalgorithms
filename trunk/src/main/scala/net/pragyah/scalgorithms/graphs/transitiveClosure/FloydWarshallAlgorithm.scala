package net.pragyah.scalgorithms.graphs.transitiveClosure

/*Compute tij =  1 if there exists a path from i to j,  0 otherwise.
 * Use Floyd-Warshall, but with(OR,AND)instead  of(min, +):
 * tij(k) = tij(k1) OR (tik(k1) AND tkj(k1)). 
 * Running Time THETA (V3)
*/
class FloydWarshallAlgorithm[A] extends TransitiveClosureAlgorithm[A] {
  
  def compute(graph:Graph[A]) : Array[Array[boolean]] = {
    val n = graph.vertices.size
    val T = new Array[Array[boolean]](n,n)
    
    graph.edges.foreach(e => T(graph.vertices.indexOf(e.v1))(graph.vertices.indexOf(e.v2)) = true)
    for( i <- 0 to (n-1))
      T(i)(i) = true
    
    
    for(k <- 0 to (n-1)){
      for(i <- 0 to (n-1)){
        for(j <- 0 to (n-1)){
          T(i)(j) = T(i)(j) || (T(i)(k) && T(k)(j)) 
        }
      }
    }
    T
  }


}
