package net.pragyah.scalgorithms.graphs.search

import scala.collection.mutable.HashMap

import junit.framework.TestCase
import junit.framework.Assert._

class TestBFS extends TestCase{
  
  
  def testGraphCLRS_Chap22 = {
    
    val (graph,solutionMap) = bfsGraphCLRS_Chap22
    var tree = new BFS().bftree(graph)
    
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
  

  
   def bfsGraphCLRS_Chap22(): (Graph[char],HashMap[Vertex[char],List[Edge[char]]]) = {
    //graph definition from CLRS chapter 22 page 533
    val vr =  Vertex[char]('r');
    val vs =  Vertex[char]('s');
    val vt =  Vertex[char]('t');
    val vu =  Vertex[char]('u');
    val vv =  Vertex[char]('v');
    val vw =  Vertex[char]('w');
    val vx =  Vertex[char]('x');
    val vy =  Vertex[char]('y');
    
    val vertices:List[Vertex[char]] = vr :: vs :: vt :: vu :: vv :: vw :: vx :: vy  :: Nil 
    val graph:Graph[char] = Graph(vertices,false)
    
    graph.addEdge(vr,vs)
    graph.addEdge(vr,vv)
    graph.addEdge(vs,vw)
    graph.addEdge(vt,vw)
    graph.addEdge(vt,vx)
    graph.addEdge(vt,vu)
    graph.addEdge(vu,vx)
    graph.addEdge(vu,vy)
    graph.addEdge(vw,vx)
    graph.addEdge(vx,vy)
    
    
    val solutionMap = new HashMap[Vertex[char],List[Edge[char]]]

    solutionMap += vr -> (graph.getEdge(vr,vs).get ::graph.getEdge(vr,vv).get :: Nil)
    solutionMap += vs -> (graph.getEdge(vs,vw).get ::Nil)
    solutionMap += vw -> (graph.getEdge(vw,vt).get ::graph.getEdge(vw,vx).get  :: Nil)
    solutionMap += vt -> (graph.getEdge(vt,vu).get ::Nil)
    solutionMap += vx -> (graph.getEdge(vx,vy).get ::Nil)
    
    (graph,solutionMap)

  }


}
