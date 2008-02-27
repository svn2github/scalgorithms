package net.pragyah.scalgorithms.graphs.shortestpath.singlesource

import scala.collection.mutable.HashMap

import junit.framework.TestCase
import junit.framework.Assert._

class TestDAG_ShortestPath extends TestCase{

  def testCLRSGraph = {
    val ssspAlgo = new DAG_ShortestPath[char]()
    val (graph,s,solutionMap) = ssspDagGraphCLRS_Chap24
    val tree = ssspAlgo.getShortestPath(graph,s)

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

  def ssspDagGraphCLRS_Chap24: (Graph[char],Vertex[char],HashMap[Vertex[char],List[Edge[char]]]) = {
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
        
    val solutionMap = new HashMap[Vertex[char],List[Edge[char]]]

    solutionMap += vs -> (graph.getEdge(vs,vt).get ::graph.getEdge(vs,vx).get  :: Nil)
    solutionMap += vx -> (graph.getEdge(vx,vy).get ::Nil)
    solutionMap += vy -> (graph.getEdge(vy,vz).get ::Nil)

    (graph,vs,solutionMap)
    
  }
 
}
