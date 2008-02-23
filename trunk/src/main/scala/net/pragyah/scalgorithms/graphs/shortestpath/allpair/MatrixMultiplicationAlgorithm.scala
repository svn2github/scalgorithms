package net.pragyah.scalgorithms.graphs.shortestpath.allpair

import net.pragyah.scalgorithms.trees.Tree

class MatrixMultiplicationAlgorithm[A] extends DynamicProgrammingAlgorithm[A]{

  override def getShortestPaths(graph:Graph[A]): List[Tree[Vertex[A]]] = {
    val W = weightMatrix(graph)/* get the weight matrix   THETA (V + E)
                                      * The weight matrix starts with the weights of the known paths in between vertices 
                                      * so far - which is nothing but the weight of direct edges. Here no indirect paths are considered 
                                      * as they are not yet discovered and thus this is just a edge-weight matrix rather than a shortest path matrix
                                      * Thus it has no weights assigned to the paths that are not single-edged  
                                      */
    val P = predecessorMatrix(graph) /* get the predecessor matrix THETA(E)
                                      * This again is a very tight matrix to begin with - with only direct paths
                                      * registered so far ... this would evolve with the run of the algorithm to give the
                                      * 'shortest-path' 
                                      */ 
    val n = W.size
    var L = W
    
    var m = 1

    //since the DynamicProgrammingAlgorithm can be looked up as a matrix multiplication ... 
    //here's matrix-multiplication 'type' solution 
    
    // lg-V* THETA(V^3) => THETA(lg V * V^3)
    while(m < n-1){
      L = extendedShortestPaths(L,L,P) //THETA(V^3)  --- Lookup the second leg from the weight matrix computed so far
      m *= 2 // thus bringing down the loop to log-n times - from n 
    }
    null
    
  }


}
