package net.pragyah.scalgorithms.graphs.mst

import net.pragyah.scalgorithms.trees.Tree

import scala.collection.jcl.TreeSet;
import scala.collection.mutable.PriorityQueue;

/* Algorithm for Connected Undirected Weighted Graphs
 * Greedy strategy - augments the tree continuously by adding the next min-weighted edge on its path.
 * 
 *  Running Time O(E.+ V lg V) Using Fibonacci heap
 * 
 * Spreads like a greedy-cancer  
 */
class PrimsAlgorithm[A] extends MSTAlgo[A] {
  
  type tagClass = Tuple2[double,Option[Vertex[A]]]  // value,parent,inQueue-Flag

  implicit def vertexToOrdered(thisVertex:Vertex[A]) : Ordered[Vertex[A]] = new Ordered[Vertex[A]]{
    def compare(thatVertex:Vertex[A]):Int = {
      (thatVertex.tag.asInstanceOf[tagClass]._1 - thisVertex.tag.asInstanceOf[tagClass]._1).toInt
    }
  }
  
    def generateMST(graph:Graph[A]): Tree[Vertex[A]] = {
      
     val vertices = graph.vertices

     var Q = new PriorityQueue[Vertex[A]]() 

     vertices.foreach(v => {v.tag = (Math.MAX_DOUBLE,None)})

      val r = vertices.head
      r.tag = (0d,None);
      
      vertices.foreach(Q+=_)
      
      while(!Q.isEmpty) { 	
        
        val u = Q.dequeue
        
        val adj1 = u.adjacent() //get all the adjacent nodes that are still in the Queue and are farther away from the tree so far than from u  
          val adj = adj1.filter(v => {
           graph.getEdge(u,v) match{
           case e:Some[Edge[A]] => Q.contains(v) && v.tag.asInstanceOf[tagClass]._1 > e.get.weight
           case _ => false
           }})
        
          adj.foreach(v => {
            v.tag = (graph.getEdge(u,v).get.weight,Some(u)) //bring all such nodes closer to the tree ... relax them 
          }
          )
        
        //PrioritQueue does not provide a remove method .. below is an ugly code that empties the queue and creates afresh
        val tempQ = Q.toQueue
        Q.clear
        Q ++= tempQ
        
      }
      
      TreeGenerator.generateTrees[A](graph,getVertexPredecessor _).head

    }
    
    def getVertexPredecessor(v:Vertex[A]) : Option[Vertex[A]] = v.tag.asInstanceOf[tagClass]._2

}
