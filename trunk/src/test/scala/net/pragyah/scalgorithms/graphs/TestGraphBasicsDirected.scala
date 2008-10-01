package net.pragyah.scalgorithms.graphs

import junit.framework.TestCase
import junit.framework.Assert._

class TestGraphBasicsDirected extends TestCase{
  
  def testVertices = {
    val v1 = Vertex[int](1);
    val v2 =  Vertex[int](2);
    val v3 =  Vertex[int](3);
    val v4 =  Vertex[int](4);
    
    val vertices:List[Vertex[int]] = v1 :: v2 :: v3 :: v4 ::  Nil 
    val graph:Graph[int] = Graph(vertices,true)

    graph.addEdge(v1,v2,9)
    graph.addEdge(v1,v4,19)
    graph.addEdge(v2,v3,12)
    graph.addEdge(v3,v4,6)
    graph.addEdge(v4,v2,5)
    graph.addEdge(v4,v1,14)
    
    
    val v1Edges = v1.getEdges
    assert(v1Edges.length == 2)
    
    val v2Edges = v2.getEdges
    assert(v2Edges.length == 1)

    val v3Edges = v3.getEdges
    assert(v3Edges.length == 1)

    val v4dges = v4.getEdges
    assert(v4dges.length == 2)

  }
  
  def testEdges = {
   val v1 = Vertex[int](1);
    val v2 =  Vertex[int](2);
    val v3 =  Vertex[int](3);
    val v4 =  Vertex[int](4);
    
    val vertices:List[Vertex[int]] = v1 :: v2 :: v3 :: v4 ::  Nil 
    val graph:Graph[int] = Graph(vertices,true)

    graph.addEdge(v1,v2,9)
    graph.addEdge(v1,v4,19)
    graph.addEdge(v2,v3,12)
    graph.addEdge(v3,v4,6)
    graph.addEdge(v4,v2,5)
    graph.addEdge(v4,v1,14)
    

    assert(graph.getEdge(v1,v2).get.weight == 9)
    assert(graph.getEdge(v2,v1) == None)

    assert(graph.getEdge(v2,v3).get.weight == 12)
    assert(graph.getEdge(v3,v2) == None)

    assert(graph.getEdge(v3,v4).get.weight == 6)
    assert(graph.getEdge(v4,v3) == None)

    assert(graph.getEdge(v4,v2).get.weight == 5)
    assert(graph.getEdge(v2,v4) == None)

    assert(graph.getEdge(v4,v1).get.weight == 14)
    assert(graph.getEdge(v1,v4).get.weight == 19)
    
    val edge = graph.getEdge(v1,v2)
    assert(edge != None)
    assert(v1.edges.contains(edge.get))
    assert(!v2.edges.contains(edge))
    graph.removeEdge(v1,v2)
    assert(graph.getEdge(v1,v2) == None)
    assert(!v1.edges.contains(edge))
    

  }
  
  def testTranspose = {
    val v1 = Vertex[int](1);
    val v2 =  Vertex[int](2);
    val v3 =  Vertex[int](3);
    val v4 =  Vertex[int](4);
    
    val vertices:List[Vertex[int]] = v1 :: v2 :: v3 :: v4 ::  Nil 
    val graph:Graph[int] = Graph(vertices,true)

    graph.addEdge(v1,v2,9)
    graph.addEdge(v1,v4,19)
    graph.addEdge(v2,v3,12)
    graph.addEdge(v3,v4,6)
    graph.addEdge(v4,v2,5)
    graph.addEdge(v4,v1,14)
   
    val graph2 = graph.transpose(false)
    
    assert(graph2.getEdge(v2,v1).get.weight == 9)
    assert(graph2.getEdge(v1,v2) == None)

    assert(graph2.getEdge(v3,v2).get.weight == 12)
    assert(graph2.getEdge(v2,v3) == None)

    assert(graph2.getEdge(v4,v3).get.weight == 6)
    assert(graph2.getEdge(v3,v4) == None)

    assert(graph2.getEdge(v2,v4).get.weight == 5)
    assert(graph2.getEdge(v4,v2) == None)

    assert(graph2.getEdge(v1,v4).get.weight == 14)
    assert(graph2.getEdge(v4,v1).get.weight == 19)

    
  }
  
}
