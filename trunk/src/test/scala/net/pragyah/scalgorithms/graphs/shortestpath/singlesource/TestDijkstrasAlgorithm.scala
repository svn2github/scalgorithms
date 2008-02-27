package net.pragyah.scalgorithms.graphs.shortestpath.singlesource

import scala.collection.mutable.HashMap

import junit.framework.TestCase
import junit.framework.Assert._

class TestDijkstras extends TestCase{
  
  def testCLRSGraph = {
    var ssspAlgo = new DijkstrasAlgorithm[char]()

    val (graph,s,solutionMap) = ssspGraphCLRS_Chap24
    
    var tree = ssspAlgo.getShortestPath(graph,s)
     tree.root.traversePreOrder(tree.root.data)((z,a) => {
                                           if(z != a){
                                             val edge = graph.getEdge(z,a)
                                             assert(edge  != None) // verify that such an edge exists
                                             assert(solutionMap(z).contains(edge.get)," Edge between "+z.data + " & "+a.data+" should not appear in the solution")
                                             solutionMap.update(z,solutionMap(z).remove(e =>e == edge.get))
                                             if(solutionMap(z) == Nil){
                                               solutionMap -= z
                                             }
                                           }
                                           a
                                      })
     assert(solutionMap.size == 0)

  }
  
  
  
  def testOCWGraph = {
    var ssspAlgo = new DijkstrasAlgorithm[char]()

    val (graph,s,solutionMap) = ssspGraphOCW_lec17
    
    var tree = ssspAlgo.getShortestPath(graph,s)
     tree.root.traversePreOrder(tree.root.data)((z,a) => {
                                           if(z != a){
                                             val edge = graph.getEdge(z,a)
                                             assert(edge  != None) // verify that such an edge exists
                                             assert(solutionMap(z).contains(edge.get)," Edge between "+z.data + " & "+a.data+" should not appear in the solution")
                                             solutionMap.update(z,solutionMap(z).remove(e =>e == edge.get))
                                             if(solutionMap(z) == Nil){
                                               solutionMap -= z
                                             }
                                           }
                                           a
                                      })
     assert(solutionMap.size == 0)

  }
  
  
  def ssspGraphCLRS_Chap24: (Graph[char],Vertex[char],HashMap[Vertex[char],List[Edge[char]]]) = {
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
    
    val solutionMap = new HashMap[Vertex[char],List[Edge[char]]]

    solutionMap += vs -> (graph.getEdge(vs,vy).get  :: Nil)
    solutionMap += vy -> (graph.getEdge(vy,vz).get ::graph.getEdge(vy,vt).get  :: Nil)
    solutionMap += vt -> (graph.getEdge(vt,vx).get ::Nil)

    (graph,vs,solutionMap)
    
  }


 
  
  def ssspGraphOCW_lec17(): (Graph[char],Vertex[char],HashMap[Vertex[char],List[Edge[char]]]) = {
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
    
    val solutionMap = new HashMap[Vertex[char],List[Edge[char]]]

    solutionMap += va -> (graph.getEdge(va,vc).get  :: Nil)
    solutionMap += vc -> (graph.getEdge(vc,vb).get ::graph.getEdge(vc,ve).get  :: Nil)
    solutionMap += vb -> (graph.getEdge(vb,vd).get ::Nil)

    (graph,va,solutionMap)
    
  }
 
  
}
