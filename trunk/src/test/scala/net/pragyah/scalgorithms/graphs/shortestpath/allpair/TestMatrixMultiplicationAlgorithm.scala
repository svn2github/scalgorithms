package net.pragyah.scalgorithms.graphs.shortestpath.allpair

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.HashMap

class TestMatrixMultiplicationAlgorithm extends TestCase{
  
  def testAPSPAlgo = {
    val apspAlgo = new MatrixMultiplicationAlgorithm[int]()
    val graph = apspGraphCLRS_Chap25
    val trees = apspAlgo.getShortestPaths(graph)
    println("Matrix Multiplication Algorithm")
   // trees.foreach(println)
    println(" INCOMPLETE - Not generating trees from predecessor matrix yet")
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
}
  
