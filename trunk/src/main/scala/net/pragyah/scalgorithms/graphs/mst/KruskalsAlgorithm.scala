package net.pragyah.scalgorithms.graphs.mst


import scala.collection.mutable.HashMap;
import scala.collection.mutable.PriorityQueue

import net.pragyah.scalgorithms.trees.Tree;


/*Enqueues all the edges in an ascending order of their weights ( in a Priority Queue). 
 * Assigns each Vertex to a distinct disjoint-set. 
 * Dequeues edges from the priority-queue one at a time
 * and merges the disjoint-sets for both the vertices (if not same already).  
 * Running Time O(E.lgV)
 */
class KruskalsAlgorithm[A] extends MSTAlgo[A] {

  
    implicit def edgeToOrdered(thisEdge:Edge[A]) : Ordered[Edge[A]] = new Ordered[Edge[A]]{
      def compare(thatEdge:Edge[A]):Int = {
        (thatEdge.weight - thisEdge.weight) match{
        case x  if(x > 0) => 1
        case x  if(x == 0) => 0
        case _ => -1
        }
      }
    }

    def generateMST(graph:Graph[A]): Tree[Vertex[A]] = {

      var A:List[Edge[A]] = List()
      val map = new HashMap[Vertex[A],int]() //using a map to map the vertex with the id of the 'imaginary' set it is associated with...
      // didn't get any good disjoint-set datastructure .. will code it later
      
      var count:int = 0
      
      graph.vertices.foreach(v => 
        {map += ((v,count)) 
        count += 1} 
      )

      val Q = new PriorityQueue[Edge[A]]() 
      Q ++= graph.edges
      
      while(!Q.isEmpty){
        val edge = Q.dequeue
        (edge.v1,edge.v2) match{
            case (v1,v2) if(map.get(v1).get != map.get(v2).get) => {val temp =  map.get(v2).get;
                                                                    map.transform( (v,c) => { //this is the UNION part .. to b e replaced later with a disjoint set's union method
                                                                                if(map.get(v).get == temp){
                                                                                  //updating the 'imaginary' set id for all the co-set-ers of v2(inclusive) to that of v1
                                                                                  map.get(v1).get
                                                                                }
                                                                                else
                                                                                   map.get(v).get 
                                                                              })
                                                                   A = A ::: List(edge)
                                                               }
            case _ => 
        
        }
     }
        
      TreeGenerator.generateTrees(graph,A).head
    }  

}
