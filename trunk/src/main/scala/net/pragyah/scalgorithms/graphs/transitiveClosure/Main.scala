package net.pragyah.scalgorithms.graphs.transitiveClosure

import scala.StringBuilder

object Main {
  
  implicit def closureMatrixToString(closure:Array[Array[boolean]]) : String = {
    
    val sb = new StringBuilder()
    for( i <- 1 to closure(0).length)
      sb.append("\t").append(i)
    
    for( j <- 0 to closure.length-1){
      sb.append("\n").append(j+1)
      closure(j).foreach(sb.append("\t").append(_))
    }
   
    sb.toString
    
  }
  
  def main(args : Array[String]) : Unit = {
    var tranCloAlgo:TransitiveClosureAlgorithm[int] = new FloydWarshallAlgorithm[int]()

    var graph = transitiveClosureGraphCLRS_Chap25
    var closure = tranCloAlgo.compute(graph)
    closure

    graph = apspGraphCLRS_Chap25
    closure = tranCloAlgo.compute(graph)
    println("Floyd-Warshall Algorithm" )
    println(graph)
    println(closureMatrixToString(closure))
    println()
    
    var tranCloAlgo2 = new FloydWarshallAlgorithm[String]()
    var graph2 = topologicalSortingGraphCLRS_Chap22
    closure = tranCloAlgo2.compute(graph2)
    println("Floyd-Warshall Algorithm" )
    println(graph2)
    println(closureMatrixToString(closure))
  }
    
    
  def transitiveClosureGraphCLRS_Chap25: Graph[int] = {
    val v1 = Vertex[int](1);
    val v2 = Vertex[int](2);
    val v3 = Vertex[int](3);
    val v4 = Vertex[int](4);

	val vertices:List[Vertex[int]] = v1 :: v2 :: v3 :: v4 :: Nil 
 
    val graph:Graph[int] = Graph[int](vertices,true) 
    graph.addEdge(v2,v3)
    graph.addEdge(v2,v4)
    graph.addEdge(v3,v2)
    graph.addEdge(v4,v1)
    graph.addEdge(v4,v3)
    
    graph
    
  }

  def apspGraphCLRS_Chap25: Graph[int] = {
    val v1 = Vertex[int](1);
    val v2 = Vertex[int](2);
    val v3 = Vertex[int](3);
    val v4 = Vertex[int](4);
    val v5 = Vertex[int](5);

	val vertices:List[Vertex[int]] = v1 :: v2 :: v3 :: v4 :: v5 :: Nil 
 
    val graph:Graph[int] = Graph[int](vertices,true) 
    graph.addEdge(v1,v2,3)
    graph.addEdge(v1,v3,8)
    graph.addEdge(v1,v5,-4)
    graph.addEdge(v2,v4,1)
    graph.addEdge(v2,v5,7)
    graph.addEdge(v3,v2,4)
    graph.addEdge(v4,v1,2)
    graph.addEdge(v4,v3,-5)
    graph.addEdge(v5,v4,6)
    
    graph
    
  }

  def topologicalSortingGraphCLRS_Chap22(): Graph[String] = {
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
    
    graph
    
  }


}
