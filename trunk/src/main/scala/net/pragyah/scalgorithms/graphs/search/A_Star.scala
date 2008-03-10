package net.pragyah.scalgorithms.graphs.search

import scala.collection.mutable.{HashSet,HashMap}

import net.pragyah.scalgorithms.trees.{Tree,Node}
import net.pragyah.scalgorithms.heaps.FibonacciHeap

class A_Star[A] {

  type searchNodeClass = Tuple3[double,double,List[Vertex[A]]] //f,g,path
  
  implicit def searchNodeClassToOrdered(thisSearchNode:searchNodeClass) : Ordered[searchNodeClass] = new Ordered[searchNodeClass]{
    def compare(thatSearchNode:searchNodeClass):Int = {
      //println("comparing : "+thisVertex.data+" - "+thatVertex.data)
      thatSearchNode._1 - thisSearchNode._1 match {
      case d if(d> 0) => -1
      case d if(d< 0) => 1
      case _ => 0
      }
    }
  }

  
  def search(graph:Graph[A],s:Vertex[A],goal:(Vertex[A]) => boolean,h:(Vertex[A]) => double): List[Vertex[A]] = {
    assume(graph.vertices.contains(s))
    s.tag = (0,Nil)
    val Q = new FibonacciHeap[searchNodeClass]((-1,0,Nil))
    Q += (0,0,List(s))
    
    val expanded = new HashSet[Vertex[A]]()
    val Qmap = new HashMap[Vertex[A],searchNodeClass]()
    
    while(!Q.empty){
      val N = Q.extractMin.get
      val u = N._3.head
      Qmap -= u
      if(goal(u)){
        return  N._3.reverse 
      }
      //no point expanding u if it has already been expanded on some other route ... 
      if(!expanded.contains(u)){
          expanded += u
	      val g = _g(graph,u,N._2)_
	      val adj = u.adjacent
	      adj.foreach( v => if(!expanded.contains(v)){
	                     val gVal = g(v)
	                     val f = gVal + h(v)
                         val Nv = (f,gVal,v::N._3)
                         if(Qmap.contains(v)){// if there alreay exits a path to this node from some other route
                           if(Qmap(v)._2 > gVal){ // and the cost of that path is greater than this one  
                             // then remove that path from the Queue
                             Q -= Qmap(v)
                             Qmap -= v 
                             // and add the new path to the Queue
		                     Q += Nv
	                         Qmap += v -> Nv 
                           }
                         }else{
		                     Q += Nv
	                         Qmap += v -> Nv 
                         }
	      })
	      
      }
    }
    
    assume(false,"No Solution")
    null
  }
  
  def _g(graph:Graph[A],u:Vertex[A],g:double)(v:Vertex[A]) : double = {
    assume(graph.getEdge(u,v) != None)
    return g + graph.getEdge(u,v).get.weight
  }

}
