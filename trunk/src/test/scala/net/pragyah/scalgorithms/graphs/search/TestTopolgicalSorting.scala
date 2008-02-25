package net.pragyah.scalgorithms.graphs.search

import junit.framework.TestCase
import junit.framework.Assert._

class TestTopolgicalSorting  extends TestCase{
  
  def testTopologicalSorting = {
    val (graph,solutionList) =  topologicalSortingGraphCLRS_Chap22
    val sorted = new DFS().topologicallySort(graph)

    assume(sorted.length == 9)
    solutionList.foreach( l =>{
                            val l2 = sorted.filter(l.contains(_))
                            assert(l.equals(l2))
                          })
  }


 def topologicalSortingGraphCLRS_Chap22(): (Graph[String],List[List[Vertex[String]]]) = {
    val undershorts = Vertex[String]("undershorts");
    val pants = Vertex[String]("pants");
    val belt = Vertex[String]("belt");
    val shirt = Vertex[String]("shirt");
    val tie = Vertex[String]("tie");
    val jacket = Vertex[String]("jacket");
    val socks = Vertex[String]("socks");
    val shoes = Vertex[String]("shoes");
    val watch = Vertex[String]("watch");

	val vertices:List[Vertex[String]] = undershorts::pants::belt::shirt::tie::jacket::socks::shoes::watch::Nil 
 
    val graph:Graph[String] = Graph[String](vertices,true) 
    graph.addEdge(undershorts,pants)
    graph.addEdge(socks,shoes)
    graph.addEdge(undershorts,shoes)
    graph.addEdge(pants,shoes)
    graph.addEdge(pants,belt)
    graph.addEdge(shirt,belt)
    graph.addEdge(shirt,tie)
    graph.addEdge(belt,jacket)
    graph.addEdge(tie,jacket)

    
    
    val order1 = socks::shoes::Nil
    val order2 = undershorts::pants::shoes::Nil
    val order3 = undershorts::shoes::Nil
    val order4 = pants::belt::jacket::Nil
    val order5 = watch::Nil
    val order6 = shirt::belt::jacket::Nil
    val order7 = shirt::tie::jacket::Nil
    val solutionList = order1 ::order2 :: order3 :: order4 :: order5 :: order6 :: order7 :: Nil 
    
    (graph,solutionList)
    
  }


}



  