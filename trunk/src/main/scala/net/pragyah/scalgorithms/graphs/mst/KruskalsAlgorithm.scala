package net.pragyah.scalgorithms.graphs.mst


import scala.collection.mutable.HashMap;
import scala.collection.mutable.PriorityQueue

import net.pragyah.scalgorithms.trees.Tree;
import net.pragyah.scalgorithms.disjointsets.DisjointSetForest


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
      val ds = new DisjointSetForest[Vertex[A]]()
      
      graph.vertices.foreach(v => ds.makeSet(v)) 

      val Q = new PriorityQueue[Edge[A]]() 
      Q ++= graph.edges
      
      while(!Q.isEmpty){
        val edge = Q.dequeue
        (edge.v1,edge.v2) match{
            case (v1,v2) if(ds(v1) != ds(v2)) => { 
                                                      ds.union(v1,v2);
                                                      A = A ::: List(edge)
                                                   }
            case _ =>         
        }
     }

      TreeGenerator.generateTrees(graph,A).head
    }  

}
