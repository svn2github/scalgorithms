package net.pragyah.scalgorithms.graphs.transitiveClosure

import junit.framework.TestCase
import junit.framework.Assert._

class TestTransitiveClosureFloydWarshalAlgo extends TestCase{
  
  def testTransitiveClosureAlgo = {
    var tranCloAlgo = new FloydWarshallAlgorithm[String]()

    var (graph,solution) = topologicalSortingGraphCLRS_Chap22
    val closure = tranCloAlgo.compute(graph)

    assert(closure.deepEquals(solution))
  
  }

   def topologicalSortingGraphCLRS_Chap22(): (Graph[String],Array[Array[boolean]]) = {
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
    
    val solution = new Array[Array[boolean]](9,9)
    solution(0) = (true::true::true::false::false::true::false::true::false::Nil).toArray
    solution(1) = (false::true::true::false::false::true::false::true::false::Nil).toArray
    solution(2) = (false::false::true::false::false::true::false::false::false::Nil).toArray
    solution(3) = (false::false::true::true::true::true::false::false::false::Nil).toArray
    solution(4) = (false::false::false::false::true::true::false::false::false::Nil).toArray
    solution(5) = (false::false::false::false::false::true::false::false::false::Nil).toArray
    solution(6) = (false::false::false::false::false::false::true::true::false::Nil).toArray
    solution(7) = (false::false::false::false::false::false::false::true::false::Nil).toArray
    solution(8) = (false::false::false::false::false::false::false::false::true::Nil).toArray

    
    (graph,solution)
    
  }

}
