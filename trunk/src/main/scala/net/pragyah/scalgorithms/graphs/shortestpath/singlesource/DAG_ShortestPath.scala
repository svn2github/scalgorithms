
package net.pragyah.scalgorithms.graphs.shortestpath.singlesource

import net.pragyah.scalgorithms.trees.Tree
import net.pragyah.scalgorithms.graphs.search.DFS

import scala.collection.mutable.PriorityQueue

/* This algorithm computes shortest paths from 
 * a single source in a Directed Acyclic Graph
 * Running Time : THETA (V + E)
 * 
 */

class DAG_ShortestPath[A] extends ShortestPathAlgo[A]{

      type dfs_tagClass = Tuple4[String,int,int,Option[Vertex[A]]]
      type tagClass = Tuple2[double,Option[Vertex[A]]]

  implicit def vertexToOrdered(thisVertex:Vertex[A]) : Ordered[Vertex[A]] = new Ordered[Vertex[A]]{
    def compare(thatVertex:Vertex[A]):Int = {
      //println("comparing : "+thisVertex.data+" - "+thatVertex.data)
      thatVertex.tag.asInstanceOf[dfs_tagClass]._3 - thisVertex.tag.asInstanceOf[dfs_tagClass]._3 match {
      case d if(d> 0) => -1
      case d if(d< 0) => 1
      case _ => 0
      }
    }
  }
  
    implicit def tagToDelta(tag:Tuple2[double,Option[Vertex[A]]]) = tag._1 


    def getShortestPath(graph:Graph[A], s:Vertex[A]): Tree[Vertex[A]] = {
      
      new DFS().dftree(graph,s)  // THETA(V + E)
      //topologically sorting
      val Q = new PriorityQueue[Vertex[A]]() 
      Q ++= graph.vertices
      val ordered = Q.toList //topologically sorted list of vertices

      graph.vertices.foreach(_.tag = (Double.MaxValue,None))
      s.tag = (0d,None)
      //relax all outgoing edges of all the vertices traversed in topologically sorted order
      ordered.foreach(u => 
        {
          u.edges.foreach( e =>
            {
              if(e.v2.tag.asInstanceOf[tagClass] - e.v1.tag.asInstanceOf[tagClass] > e.weight)
                e.v2.tag = (e.v1.tag.asInstanceOf[tagClass] + e.weight,Some(e.v1)) // relax edges
            }
          )
        }
      )
      
      TreeGenerator.generateTrees[A](graph, getVertexPredecessor _).filter(_.root.data == s).head
    }

  def getVertexPathWeight(v:Vertex[A]) : double = v.tag.asInstanceOf[tagClass]._1
  def getVertexPredecessor(v:Vertex[A]) : Option[Vertex[A]] = v.tag.asInstanceOf[tagClass]._2

}
