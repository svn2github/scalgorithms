package net.pragyah.scalgorithms.graphs.shortestpath.allpair

import scala.collection.mutable.HashMap

import junit.framework.TestCase
import junit.framework.Assert._

 
class TestJohnsonsAlgorithm extends TestCase{
  
  def testAPSPAlgo = {
    val apspAlgo = new JohnsonsAlgorithm[int]()
    val (graph,solutionMapMap) = apspGraphCLRS_Chap25
    val trees = apspAlgo.getShortestPaths(graph)
     
    trees.foreach(tree => {
                    assert(solutionMapMap(tree.root.data) != null)
                    val solutionMap = solutionMapMap(tree.root.data)
                    tree.root.traversePreOrder(tree.root.data)((z,a) => {
                                               if(z != a){
                                                 assert(solutionMap.contains(z)," Vertex not in tree  "+z)
                                                 val edge = graph.getEdge(z,a)
                                                 assert(edge  != None) // verify that such an edge exists
                                                 assert(solutionMap(z).contains(edge.get)," Edge between "+z.data + " & "+z.data+" should not appear in the solution")
                                                 solutionMap.update(z,solutionMap(z).remove(e =>e == edge.get))
                                                 if(solutionMap(z) == Nil){
                                                   solutionMap -= z
                                                 }
                                               }
                                               a
                                          })
                  })
    
    trees.foreach(tree => assert(solutionMapMap(tree.root.data).size == 0))
  }
  
  def apspGraphCLRS_Chap25: (Graph[int],HashMap[Vertex[int],HashMap[Vertex[int],List[Edge[int]]]]) = {
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
    
    val solutionMap = new HashMap[Vertex[int],HashMap[Vertex[int],List[Edge[int]]]]()

    val v1Map = new HashMap[Vertex[int],List[Edge[int]]]
    v1Map += v1 -> (graph.getEdge(v1,v5).get :: Nil)
    v1Map += v5 -> (graph.getEdge(v5,v4).get ::Nil)
    v1Map += v4 -> (graph.getEdge(v4,v3).get :: Nil)
    v1Map += v3 -> (graph.getEdge(v3,v2).get :: Nil)
    solutionMap += v1 -> v1Map
    
    val v2Map = new HashMap[Vertex[int],List[Edge[int]]]
    v2Map += v2 -> (graph.getEdge(v2,v4).get :: Nil)
    v2Map += v4 -> (graph.getEdge(v4,v3).get ::graph.getEdge(v4,v1).get ::Nil)
    v2Map += v1 -> (graph.getEdge(v1,v5).get :: Nil)
    solutionMap += v2 -> v2Map
    
    val v3Map = new HashMap[Vertex[int],List[Edge[int]]]
    v3Map += v3 -> (graph.getEdge(v3,v2).get :: Nil)
    v3Map += v2 -> (graph.getEdge(v2,v4).get ::Nil)
    v3Map += v4 -> (graph.getEdge(v4,v1).get :: Nil)
    v3Map += v1 -> (graph.getEdge(v1,v5).get :: Nil)
    solutionMap += v3 -> v3Map
    
    val v4Map = new HashMap[Vertex[int],List[Edge[int]]]
    v4Map += v4 -> (graph.getEdge(v4,v1).get :: graph.getEdge(v4,v3).get :: Nil)
    v4Map += v1 -> (graph.getEdge(v1,v5).get :: Nil)
    v4Map += v3 -> (graph.getEdge(v3,v2).get :: Nil)
    solutionMap += v4 -> v4Map
    
    val v5Map = new HashMap[Vertex[int],List[Edge[int]]]
    v5Map += v5 -> (graph.getEdge(v5,v4).get :: Nil)
    v5Map += v4 -> (graph.getEdge(v4,v1).get :: graph.getEdge(v4,v3).get :: Nil)
    v5Map += v3 -> (graph.getEdge(v3,v2).get :: Nil)
    solutionMap += v5 -> v5Map
    
    (graph,solutionMap)
    
  }
}
  
