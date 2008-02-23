package net.pragyah.scalgorithms.graphs.shortestpath.allpair

 
  
import net.pragyah.scalgorithms.trees.{Tree,Node}

/* A dynamic-programming formulation to solve all-pairs shortest-path problem
 * on a directed graph
 * Allows negative weight edges but assumes non-negative weight cycles
 * 
 * Running time Theta(V^3)
 * 
 */
class FloydWarshallAlgorithm[A] extends ShortestPathAlgo[A] {

      def getShortestPaths(graph:Graph[A]): List[Tree[Vertex[A]]] = {
         
        val W = weightMatrix(graph)  /* get the weight matrix   THETA (V + E)
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

        val n = graph.vertices.size
        
        for( k <- 0 to (n-1) ){  /*this loop is the crux of the algorithm defines 
                                  * the range of intermediate vertices in between i and j
                                  * Every single possible pair (i,j) is picked up and it is examined for the 
                                  * possibility that the shortest path discovered so far (considering V1 to Vk-1) is greater than the path via Vk
                                  * i.e. i ~> k ~> j (looking up (i,k) and (k,j) entries in the weight matrix)
                                  * if that is the case - then we have discovered a shorter route .. if not then tha path discovered so far
                                  * is the shortest so far.
                                  * This can go till all the possible intermidiate vertices are considered - v1 to vn
                                  * 
                                  * 
                                  */
	        for( i <- 0 to (n-1) ){
		        for( j <- 0 to (n-1) ){ // Clearly THETA(V^3)
		          if(W(i)(j) > W(i)(k) + W(k)(j)){ // is path discovered so far (via V1 - Vk-1) is bigger than the one through Vk
                     W(i)(j) = W(i)(k) + W(k)(j) // path thorugh Vk is the shortest path
                     P(i)(j) = P(k)(j) // Vk is the predecessor(direct or indirect) for the shortest path  - Typical DynamicProgramming
		          }
		        }
	        }
        }
        
        
        generateForest(graph,P)
      }
      
      
      
}
