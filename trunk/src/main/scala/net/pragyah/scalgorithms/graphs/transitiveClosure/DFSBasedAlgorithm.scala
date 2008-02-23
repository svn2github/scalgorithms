package net.pragyah.scalgorithms.graphs.transitiveClosure

import net.pragyah.scalgorithms.graphs.search.DFS

class DFSBasedAlgorithm[A] extends TransitiveClosureAlgorithm[A] {
  
  def compute(graph:Graph[A]) : Array[Array[boolean]] = {
    val n = graph.vertices.size
    val T = new Array[Array[boolean]](n,n)

    val dfs = new DFS[A]()
    
    graph.vertices.foreach(v => {
                              val trees = dfs.dftree(graph,v) 
                              val vtree = trees.filter( _.root.data == v).head
                              val index = graph.vertices.indexOf(v)
                              // INCOMPLETE
                              index
                           })
    
    null
  }

}
