package net.pragyah.scalgorithms.graphs

object GraphApp {
  
  def main(args : Array[String]) : Unit = {
    val v1 = Vertex[int](1);
    val v2 =  Vertex[int](2);
    val v3 =  Vertex[int](3);
    val v4 =  Vertex[int](4);
    val v5 =  Vertex[int](5);
    val v6 =  Vertex[int](6);
    val v7 =  Vertex[int](7);
    val v8 =  Vertex[int](8);
    
    val vertices:List[Vertex[int]] = v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: v7 :: v8 :: Nil 
    val graph:Graph[int] = Graph(vertices,false)
    
    graph.addEdge(v1,v2,9)
    graph.addEdge(v2,v3,12)
    graph.addEdge(v3,v4,6)
    graph.addEdge(v4,v2,5)
    graph.addEdge(v4,v5,14)
    graph.addEdge(v4,v6,3)
    graph.addEdge(v5,v6,18)
    graph.addEdge(v6,v7,10)
    graph.addEdge(v7,v2,7)
    graph.addEdge(v7,v8,15)
 
    println(graph)
    
    println("done")
  }
}
