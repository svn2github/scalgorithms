package net.pragyah.scalgorithms.graphs.shortestpath.singlesource

object Main {

  def main(args : Array[String]) : Unit = {
    
    
    var mstAlgo:ShortestPathAlgo[char] = new DijkstrasAlgorithm[char]()

    val gs = ssspGraphCLRS_Chap24
    val graph:Graph[char] =  gs._1
    val s:Vertex[char] =  gs._2
    
    var tree = mstAlgo.getShortestPath(graph,s)
    println(" Dijkstra's Algorithm")
    println(tree)
    
    val gs2 = ssspGraphOCW_lec17
    val graph2:Graph[char] =  gs2._1
    val s2:Vertex[char] =  gs2._2
    tree = mstAlgo.getShortestPath(graph2,s2)
    println(" Dijkstra's Algorithm")
    println(tree)
    
    
    mstAlgo = new BellmanFordAlgorithm[char]()
    
    val gs3 = ssspGraphOCW_lec18
    val graph3:Graph[char] =  gs3._1
    val s3:Vertex[char] =  gs3._2
    tree = mstAlgo.getShortestPath(graph3,s3)
    println(" Bellman-Ford Algorithm")
    println(tree)
    
    
    mstAlgo = new DAG_ShortestPath[char]()

    val gs4 = ssspDagGraphCLRS_Chap24
    val graph4:Graph[char] =  gs4._1
    val s4:Vertex[char] =  gs4._2
    tree = mstAlgo.getShortestPath(graph4,s4)
    println(" DAG-Shortest Path")
    println(tree)
    
    
  }
  

  def ssspGraphCLRS_Chap24: (Graph[char],Vertex[char]) = {
    val vs = Vertex[char]('s');
    val vt = Vertex[char]('t');
    val vx = Vertex[char]('x');
    val vy = Vertex[char]('y');
    val vz = Vertex[char]('z');

	val vertices:List[Vertex[char]] = vs :: vt :: vx :: vy :: vz :: Nil 
 
    val graph:Graph[char] = Graph[char](vertices,true) 
    graph.addEdge(vs,vt,10)
    graph.addEdge(vs,vy,5)
    graph.addEdge(vt,vx,1)
    graph.addEdge(vt,vy,2)
    graph.addEdge(vx,vz,4)
    graph.addEdge(vy,vt,3)
    graph.addEdge(vy,vx,9)
    graph.addEdge(vy,vz,2)
    graph.addEdge(vz,vx,6)
    graph.addEdge(vz,vs,7)
    
    (graph,vs)
    
  }

  
  def ssspGraphOCW_lec17(): (Graph[char],Vertex[char]) = {
    val va = Vertex[char]('A');
    val vb = Vertex[char]('B');
    val vc = Vertex[char]('C');
    val vd = Vertex[char]('D');
    val ve = Vertex[char]('E');

	val vertices:List[Vertex[char]] = va :: vb :: vc :: vd :: ve :: Nil 
 
    val graph:Graph[char] = Graph[char](vertices,true) 
    graph.addEdge(va,vb,10)
    graph.addEdge(va,vc,3)
    graph.addEdge(vb ,vd,2)
    graph.addEdge(vb,vc,1)
    graph.addEdge(vc,vb,4)
    graph.addEdge(vc,vd,8)
    graph.addEdge(vc,ve,2)
    graph.addEdge(vd,ve,7)
    graph.addEdge(ve,vd,9)
    
    (graph,va)
    
  }
 
  def ssspGraphOCW_lec18(): (Graph[char],Vertex[char]) = {
    val va = Vertex[char]('A');
    val vb = Vertex[char]('B');
    val vc = Vertex[char]('C');
    val vd = Vertex[char]('D');
    val ve = Vertex[char]('E');

	val vertices:List[Vertex[char]] = va :: vb :: vc :: vd :: ve :: Nil 
 
    val graph:Graph[char] = Graph[char](vertices,true) 
    graph.addEdge(va,vb,-1)
    graph.addEdge(va,vc,4)
    graph.addEdge(vb,vc,3)
    graph.addEdge(vb,vd,2)
    graph.addEdge(vb,ve,2)
    graph.addEdge(vd,vc,5)
    graph.addEdge(vd,vb,1)
    graph.addEdge(ve,vd,-3)
    //graph.addEdge(ve,vd,-4)  // negative cycle
    
    (graph,va)
    
  }
  
  def ssspDagGraphCLRS_Chap24: (Graph[char],Vertex[char]) = {
    val vr = Vertex[char]('r');//INF
    val vs = Vertex[char]('s');//0
    val vt = Vertex[char]('t');//2
    val vx = Vertex[char]('x');//6
    val vy = Vertex[char]('y');//5
    val vz = Vertex[char]('z');//3

	val vertices:List[Vertex[char]] = vr :: vs :: vt :: vx :: vy :: vz :: Nil 
 
    val graph:Graph[char] = Graph[char](vertices,true) 
    graph.addEdge(vr,vs,5)
    graph.addEdge(vr,vt,3)
    graph.addEdge(vs,vt,2)
    graph.addEdge(vs,vx,6)
    graph.addEdge(vt,vx,7)
    graph.addEdge(vt,vy,4)
    graph.addEdge(vt,vz,2)
    graph.addEdge(vx,vy,-1)
    graph.addEdge(vx,vz,1)
    graph.addEdge(vy,vz,-2)
    
    (graph,vs)
    
  }


}
