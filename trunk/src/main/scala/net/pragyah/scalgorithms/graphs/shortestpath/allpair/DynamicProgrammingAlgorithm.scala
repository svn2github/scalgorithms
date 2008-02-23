package net.pragyah.scalgorithms.graphs.shortestpath.allpair

import net.pragyah.scalgorithms.trees.Tree

/* Dynamic Programming Algorithm for All-Pair shortest path
 * Takes THEAT(V^4) time
 */
class DynamicProgrammingAlgorithm[A] extends ShortestPathAlgo[A] {

  def getShortestPaths(graph:Graph[A]): List[Tree[Vertex[A]]] = {
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
    
    // V* THETA(V^3) => THETA(V^4)
    for(m <- 2 to (n-1)){
      L = extendedShortestPaths(L,W,P) //THETA(V^3)  --- always lookup the second leg from the edge-weight matrix
    }

    //wrong predecessor matrix getting generated .. TODO
	//generateForest(graph,P)
    Nil

  }
  
  
  def extendedShortestPaths(L:Array[Array[Option[double]]],W:Array[Array[Option[double]]],P:Array[Array[Option[int]]]) : Array[Array[Option[double]]] = {
    
    val n = L.size
    val L_ = new Array[Array[Option[double]]](n,n)
    
    for( i <- 0 to (n-1)){
      for (j <- 0 to (n-1) if(i != j)){
        //for every pair Vi and Vj ... see if there is a shorter path via any other vertex k (where the second leg k-->j is defined in weight matrix W)
        //...   Vi ~~> Vk --> Vj
        for(k <- 0 to (n-1)){ // CLEARLY THETA(V^3)
          if(L_(i)(j) > L(i)(k) + W(k)(j)){
            L_(i)(j) = L(i)(k) + W(k)(j) // if there is .. then register it ...
            P(i)(j) = k // WRONG ... TODO
          }
        }
      }
    }

    L_
  }

}
