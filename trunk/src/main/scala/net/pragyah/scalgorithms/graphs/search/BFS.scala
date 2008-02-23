package net.pragyah.scalgorithms.graphs.search

import net.pragyah.scalgorithms.trees._
import net.pragyah.scalgorithms.graphs._

/*Computes shortest path distance - for unweighted graphs. 
 * Queues up the discovered vertices, dequeues them one at a time and enqueue
 * their undiscovered children. 
 * Creates Breadth First Tree
 * Running time O (V + E)
*/
class BFS[A]{
  
  type tagClass = Tuple3[String,int,Option[Vertex[A]]]
  
  def bftree(graph:Graph[A]):Tree[Vertex[A]] = {
    
    graph.vertices.foreach(_.tag = ("WHITE",-1,None))  // set color, depth and parent
    val s = graph.vertices.head
    s.tag = ("GRAY",0,None)
    
    var queue = s::Nil
    
    while(queue.length != 0){
      val u = queue.head
      queue = queue.tail
      val utag = u.tag.asInstanceOf[tagClass]
      val whites=u.adjacent.filter( _.tag.asInstanceOf[tagClass]._1 == "WHITE") // get all not-yet-touched ones
      whites.foreach( v => {
        v.tag = ("GRAY",utag._2+1,Some(u));// mark as alredy-under-consideration 
        queue = queue ::: List(v) //and add them to the queue
        }
      )
      u.tag = ("BLACK",utag._2,utag._3) // mark complete
    }
    
    TreeGenerator.generateTrees(graph,getVertexPredecessor _).head
  }

  def getVertexDepth(v:Vertex[A]) : double = v.tag.asInstanceOf[tagClass]._2
  def getVertexPredecessor(v:Vertex[A]) : Option[Vertex[A]] = v.tag.asInstanceOf[tagClass]._3

  
}
