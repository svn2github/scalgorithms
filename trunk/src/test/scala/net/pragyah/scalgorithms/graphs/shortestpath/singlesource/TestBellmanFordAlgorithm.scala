package net.pragyah.scalgorithms.graphs.shortestpath.singlesource

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.HashMap

class TestBellmanFordAlgorithm extends TestCase{
  
  
  def testOCWGraph = {
    val mstAlgo = new BellmanFordAlgorithm[char]()
    
    val (graph,s,solutionMap) = ssspGraphOCW_lec18
    val tree = mstAlgo.getShortestPath(graph,s)

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
  
 def testNegativeWtCycle = {
    val mstAlgo = new BellmanFordAlgorithm[char]()
    
    val (graph,s) = ssspGraphOCW_lec18_NWC
    
    try{
      mstAlgo.getShortestPath(graph,s)
      assert(false,"should have thrown a java.lang.IllegalArgumentException stating that there is a Negative Weight Cycle")
    }catch{
    case ex:java.lang.IllegalArgumentException => assert(true)
    case ex:_ =>assert(false,"should have thrown a java.lang.IllegalArgumentException and not " +  ex.getClass.getName)
    }

  }
  //ocw -> http://mit.ocw.edu
  def ssspGraphOCW_lec18(): (Graph[char],Vertex[char],HashMap[Vertex[char],List[Edge[char]]]) = {
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

    val solutionMap = new HashMap[Vertex[char],List[Edge[char]]]

    solutionMap += va -> (graph.getEdge(va,vb).get  :: Nil)
    solutionMap += vb -> (graph.getEdge(vb,vc).get ::graph.getEdge(vb,ve).get  :: Nil)
    solutionMap += ve -> (graph.getEdge(ve,vd).get ::Nil)

    (graph,va,solutionMap)

  }
  //ocw -> http://mit.ocw.edu
  def ssspGraphOCW_lec18_NWC(): (Graph[char],Vertex[char]) = {
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
    graph.addEdge(ve,vd,-4)  // negative cycle

    (graph,va)

  }

}
