package net.pragyah.scalgorithms.graphs

import junit.framework.TestCase
import junit.framework.Assert._

class TestGraphBasicsUndirected extends TestCase{
  
  def testVertices = {
    val v1 = Vertex[int](1);
    val v2 =  Vertex[int](2);
    val v3 =  Vertex[int](3);
    val v4 =  Vertex[int](4);
    
    val vertices:List[Vertex[int]] = v1 :: v2 :: v3 :: v4 ::  Nil 
    val graph:Graph[int] = Graph(vertices,false)

    graph.addEdge(v1,v2,9)
    graph.addEdge(v2,v3,12)
    graph.addEdge(v3,v4,6)
    graph.addEdge(v4,v2,5)
    graph.addEdge(v4,v1,14)
    
    
    val v1Edges = v1.getEdges
    assert(v1Edges.length == 2)
    
    val v2Edges = v2.getEdges
    assert(v2Edges.length == 3)

    val v3Edges = v3.getEdges
    assert(v3Edges.length == 2)

    val v4dges = v4.getEdges
    assert(v4dges.length == 3)

  }
  
  def testEdges = {
    val v1 = Vertex[int](1);
    val v2 =  Vertex[int](2);
    val v3 =  Vertex[int](3);
    val v4 =  Vertex[int](4);
    
    val vertices:List[Vertex[int]] = v1 :: v2 :: v3 :: v4 ::  Nil 
    val graph:Graph[int] = Graph(vertices,false)

    graph.addEdge(v1,v2,9)
    graph.addEdge(v2,v3,12)
    graph.addEdge(v3,v4,6)
    graph.addEdge(v4,v2,5)
    graph.addEdge(v4,v1,14)

    assert(graph.getEdge(v1,v2).get.weight == 9)
    assert(graph.getEdge(v2,v1).get.weight == 9)

    assert(graph.getEdge(v2,v3).get.weight == 12)
    assert(graph.getEdge(v3,v2).get.weight == 12)

    assert(graph.getEdge(v3,v4).get.weight == 6)
    assert(graph.getEdge(v4,v3).get.weight == 6)

    assert(graph.getEdge(v4,v2).get.weight == 5)
    assert(graph.getEdge(v2,v4).get.weight == 5)

    assert(graph.getEdge(v4,v1).get.weight == 14)
    assert(graph.getEdge(v1,v4).get.weight == 14)

  }

  def testTranspose = {
    val v1 = Vertex[int](1);
    val v2 =  Vertex[int](2);
    val v3 =  Vertex[int](3);
    val v4 =  Vertex[int](4);
    
    val vertices:List[Vertex[int]] = v1 :: v2 :: v3 :: v4 ::  Nil 
    val graph:Graph[int] = Graph(vertices,false)

    graph.addEdge(v1,v2,9)
    graph.addEdge(v2,v3,12)
    graph.addEdge(v3,v4,6)
    graph.addEdge(v4,v2,5)
    graph.addEdge(v4,v1,14)

    try{
      graph.transpose(false)
      assert(false, "Should have thrown an exception saying that the graph is not directed")
    }catch {
	     case ex:java.lang.IllegalArgumentException => //do nothing
   	     case ex:java.lang.Exception  => assert(false," Some Exception : "+ex.getMessage())
    }
    
    
  }

}
