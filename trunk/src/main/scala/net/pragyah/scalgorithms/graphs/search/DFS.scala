package net.pragyah.scalgorithms.graphs.search

import net.pragyah.scalgorithms.trees._
import net.pragyah.scalgorithms.graphs._

/*Recursively spans the children to generate depth-first tree(s)
 * The predecessor sub-graph of a depth-first search forms a depth-first forest composed of several depth-first trees
 * Running Time THETA (V + E)
 * USES : 
 * - Topological Sorting of DAGs : One application of DFS is in performing a topological sort of a DAG. 
 * 		Topological sort of a dag is a linear ordering of all its vertices such that if graph contain an edge (u,v) then u appears before v in the ordering. 
 * 		Running time THETA (V + E)
 * - Strongly Connected Components :  A classic application of DFS is in decomposing a directed graph into its strongly connected components.
 * 		Running time THETA (V + E)
*/
class DFS[A] {
  
    type tagClass = Tuple4[String,int,int,Option[Vertex[A]]] //color, discover-time, finish-time,parent
    var time = 0
    
    def dftree(graph:Graph[A],sortF:((Vertex[A],Vertex[A]) => Boolean)):List[Tree[Vertex[A]]] = {
      var sorted = graph.vertices.sort(sortF)
      sorted.foreach(u => if (u.tag.asInstanceOf[tagClass]._1 == "WHITE") dfsVisit(u))
	  TreeGenerator.generateTrees[A](graph,getVertexPredecessor _)
    }

    def dftree(graph:Graph[A]):List[Tree[Vertex[A]]] = {
      graph.vertices.foreach(_.tag = ("WHITE",None,None,None))  // set color, discovered-timestamp, finished-timestamp and parent 
      graph.vertices.foreach(u => if (u.tag.asInstanceOf[tagClass]._1 == "WHITE") dfsVisit(u))
      	  TreeGenerator.generateTrees[A](graph,getVertexPredecessor _)

    }
    
    def dftree(graph:Graph[A],s:Vertex[A]):List[Tree[Vertex[A]]] = {
      assume(graph.vertices.exists(_ == s))
      graph.vertices.foreach(_.tag = ("WHITE",None,None,None))  // set color, discovered-timestamp, finished-timestamp and parent
      
      dfsVisit(s)
      graph.vertices.foreach(u => if (u.tag.asInstanceOf[tagClass]._1 == "WHITE") dfsVisit(u))
      	  TreeGenerator.generateTrees[A](graph,getVertexPredecessor _)

    }
    
    
    
    def dfsVisit(u:Vertex[A]):Unit = {

      var utag =  u.tag.asInstanceOf[tagClass]
      
      time += 1  
      utag = ("GRAY",time,utag._3,utag._4)   
      u.tag = utag

      u.adjacent.foreach(v => {
                                var vtag =  v.tag.asInstanceOf[tagClass]
                                if(vtag._1 == "WHITE"){
                                  v.tag = (vtag._1,vtag._2,vtag._3,Some(u)) 
                                  dfsVisit(v)
                                }
                         })
      time += 1
      u.tag = ("BLACK",utag._2,time,utag._4)   
   }
      
    def dftree_norecursion(graph:Graph[A]):List[Tree[Vertex[A]]] = {
      
        graph.vertices.foreach(_.tag = ("WHITE",None,None,None))  // set color, discovered-timestamp, finished-timestamp and parent 
        
        graph.vertices.foreach(u => if (u.tag.asInstanceOf[tagClass]._1 == "WHITE"){
          
                            var stack:List[Vertex[A]] = u::Nil

                            while(stack.length > 0){
                              val u = stack.head
                              var utag =  u.tag.asInstanceOf[tagClass]
                              val adj:List[Vertex[A]] = u.adjacent.filter(_.tag.asInstanceOf[tagClass]._1 == "WHITE")
	                              time += 1  
	                              u.tag = ("GRAY",time,utag._3,utag._4)
                              
                              if(adj.length != 0){
                                adj.foreach(v => {
                                  val vtag = v.tag.asInstanceOf[tagClass]
                                  v.tag = (vtag._1,vtag._2,vtag._3,Some(u))
                                })
                                stack = adj:::stack
                              }else{
							      time += 1
							      u.tag = ("BLACK",utag._2,time,utag._4)
                                  stack = stack.tail
                              }

                            }
                           
                         })
        
        TreeGenerator.generateTrees[A](graph,getVertexPredecessor _)

    }

  def getVertexDiscoverTime(v:Vertex[A]) : double = v.tag.asInstanceOf[tagClass]._2
  def getVertexFinishedTime(v:Vertex[A]) : double = v.tag.asInstanceOf[tagClass]._3
  def getVertexPredecessor(v:Vertex[A]) : Option[Vertex[A]] = v.tag.asInstanceOf[tagClass]._4

}
