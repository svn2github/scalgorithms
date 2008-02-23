package net.pragyah.scalgorithms.graphs.shortestpath.allpair

import scala.collection.mutable.HashMap

import net.pragyah.scalgorithms.graphs.shortestpath.singlesource.{BellmanFordAlgorithm,DijkstrasAlgorithm}
import net.pragyah.scalgorithms.trees.Tree


/* Johnson's Algorihtm is a way to solve the all-pairs shortest path problem 
 * in sparse, weighted, directed graphs
 * 
 * Uses
 * 	- BellmanFord Algorithm 
 * 	- Dijkstra's Algorithms
 *  - Graph Reweighting
 * 
 * Running time of this algorithm is O(V^2 lg V + V.E)  (when Dijkstra's algorithm runs with Fibonacci-heap)
 */


class JohnsonsAlgorithm[A] extends ShortestPathAlgo[A] {
  
    def getShortestPaths(graph:Graph[A]): List[Tree[Vertex[A]]] = {
      //create a dummy vertex with the data of the first vertex in graph ... data doesn't matter anyways
      val s = new Vertex[A](graph.vertices.head.data)
      // create a new graph with all the vertices of 'graph' + s  V[_graph] = V[graph] U {s}
      val _graph = new Graph[A](s::graph.vertices,graph.directed)
      _graph.edges = graph.edges
      //add a 0 weight edge from s to each vertex of the graph  E[_graph] = E[graph] U {(s,v) : v c V[graph]} 
      _graph.vertices.tail.foreach(_graph.addEdge(s,_,0.0))
        
      //make a run of Bellman-Ford algorithm on _graph to figure out shortest paths to all vertices from the dummy node s  
      // this also checks against any negative weight cycles
      val bf = new BellmanFordAlgorithm[A]();
      bf.getShortestPath(_graph,s)
      
      //create a temporary map
      val h = new HashMap[Vertex[A],double]()
      //and save the 'h' weights in the map ... as set by the bellman-ford algorthm 
      _graph.vertices.tail.foreach(v => h += v -> bf.getVertexPathWeight(v))
      //temporarily 'reweight' the graph to have all positive edge-weights for Dijkstra's to be able to do its job 
      graph.edges.foreach(e => e.weight = e.weight + h(e.v1) - h(e.v2))
      
      //weight matrix
      val D = new Array[Array[double]](graph.vertices.size,graph.vertices.size)
      // placeholders for shortest-path trees
      var trees = List[Tree[Vertex[A]]]()
      
      val dijsktra = new DijkstrasAlgorithm[A]()
      
      
      graph.vertices.foreach(u => { // for each vertex
          trees = dijsktra.getShortestPath(graph,u) :: trees // figure out the shortest path using Dijkstra's Algorithm and get the shortest-path tree
          val uIndex = graph.vertices.indexOf(u)
          graph.vertices.foreach(v =>{
              val vIndex = graph.vertices.indexOf(v)
              D(uIndex)(vIndex) = dijsktra.getVertexPathWeight(v) + h(v) - h(u)  // get the weight matrix for shortest paths 
              
            }
          ) 
        }
      )
      
      //reset -- for the heck of it
      graph.edges.foreach(e => e.weight = e.weight + h(e.v2) - h(e.v1))
      graph.clearTags
      
      trees
    }

}
