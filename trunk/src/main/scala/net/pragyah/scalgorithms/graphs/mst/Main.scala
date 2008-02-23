package net.pragyah.scalgorithms.graphs.mst

object Main {
  def main(args : Array[String]) : Unit = {
    
    var graph:Graph[char] =  mstGraphCLRS_chap23()
    
    var mstAlgo:MSTAlgo[char] = new PrimsAlgorithm[char]()
    var tree = mstAlgo.generateMST(graph)
    println(" Prim's MST")
    println(tree)

	graph =  mstGraphCLRS_chap23()
    mstAlgo = new KruskalsAlgorithm[char]()
    tree = mstAlgo.generateMST(graph)
    println(" Kruskal's MST")
    println(tree)
    
  }
  

  def mstGraphCLRS_chap23(): Graph[char] = {
    val va = Vertex[char]('a');
    val vb = Vertex[char]('b');
    val vc = Vertex[char]('c');
    val vd = Vertex[char]('d');
    val ve = Vertex[char]('e');
    val vf = Vertex[char]('f');
    val vg = Vertex[char]('g');
    val vh = Vertex[char]('h');
    val vi = Vertex[char]('i');

	val vertices:List[Vertex[char]] = va :: vb :: vc :: vd :: ve :: vf :: vg :: vh :: vi :: Nil 
 
    val graph:Graph[char] = Graph[char](vertices,false) 
    graph.addEdge(va,vb,4)
    graph.addEdge(va,vh,8)
    graph.addEdge(vb,vc,8)
    graph.addEdge(vb,vh,11)
    graph.addEdge(vc,vd,7)
    graph.addEdge(vc,vi,2)
    graph.addEdge(vc,vf,4)
    graph.addEdge(vd,ve,9)
    graph.addEdge(vd,vf,14)
    graph.addEdge(ve,vf,10)
    graph.addEdge(vf,vg,2)
    graph.addEdge(vg,vh,1)
    graph.addEdge(vg,vi,6)
    graph.addEdge(vh,vi,7)
    
    graph
    
  }

 
}
