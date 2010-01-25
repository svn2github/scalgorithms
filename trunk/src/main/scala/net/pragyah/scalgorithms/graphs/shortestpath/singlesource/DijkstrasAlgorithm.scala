package net.pragyah.scalgorithms.graphs.shortestpath.singlesource

import scala.collection.mutable.PriorityQueue

import net.pragyah.scalgorithms.graphs.TreeGenerator
import net.pragyah.scalgorithms.trees.Tree

/*
 * A Greedy algorithm that computes shortest-paths on a 
 * weighted, directed graph with non-negative edge weights. 
 * Spreads the shortest-path tree like a cancer engulfing
 * the closest vertices first. Works like Prim's MST algorithm.
 * 
 * Running Time = THETA (V lg V + E) Using Fibonnaci Heaps
 * 
*/
class DijkstrasAlgorithm[A] extends ShortestPathAlgo[A]{
  
  type tagClass = Tuple2[double,Option[Vertex[A]]]  // value,parent,inQueue-Flag

  implicit def vertexToOrdered(thisVertex:Vertex[A]) : Ordered[Vertex[A]] = new Ordered[Vertex[A]]{
    def compare(thatVertex:Vertex[A]):Int = {
      //println("comparing : "+thisVertex.data+" - "+thatVertex.data)
      thatVertex.tag.asInstanceOf[tagClass]._1 - thisVertex.tag.asInstanceOf[tagClass]._1 match {
      case d if(d> 0) => 1
      case d if(d< 0) => -1
      case _ => 0
      }
    }
  }

  implicit def vertexToDouble(v:Vertex[A]) : double = v.tag.asInstanceOf[tagClass]._1

  def getShortestPath(graph:Graph[A], s:Vertex[A]): Tree[Vertex[A]] = {
    
    graph.vertices.foreach(_.tag = (Double.MaxValue,None))
    s.tag = (0d,None)
    
    var S:List[Vertex[A]] = List()

    val Q = new PriorityQueue[Vertex[A]]()
    Q ++= graph.vertices
    // add all to the queue
    
    while(!Q.isEmpty){
      
      val u = Q.dequeue  // dequeue one with the shortest  ... 
      S = S ::: List(u)
      u.adjacent.foreach( v => {
        val w = graph.getEdge(u,v).get.weight
/*          if(v.tag.asInstanceOf[tagClass]._1 > u.tag.asInstanceOf[tagClass]._1 + w)*/
        val wv:Double = v //implicit vertexToDouble
        val wu:Double = u //implicit vertexToDouble
        if(wv > wu + w) //relax ... and set the predessessor 
            v.tag = (wu + w,Some(u))
      })

      //replace all this with proper heap operations
      val tempQ = Q.toQueue
      Q.clear
      Q ++= tempQ

    }
    
    TreeGenerator.generateTrees[A](graph,getVertexPredecessor _).head
  }

  def getVertexPathWeight(v:Vertex[A]) : double = v
  def getVertexPredecessor(v:Vertex[A]) : Option[Vertex[A]] = v.tag.asInstanceOf[tagClass]._2

}
