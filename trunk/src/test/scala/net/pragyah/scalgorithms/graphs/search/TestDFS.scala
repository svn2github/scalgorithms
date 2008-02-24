package net.pragyah.scalgorithms.graphs.search

import scala.collection.mutable.HashMap

import junit.framework.TestCase
import junit.framework.Assert._

class TestDFS extends TestCase{
  
  
  def testGraphCLRS_Chap22_u = {
    //test with the first vertex ... u
    val (graph,solutionMapMap) = dfsGraphCLRS_chap22
    var trees = new DFS().dftree(graph)
    assert(trees.length == 2)
    trees.foreach(tree => {
                    assert(solutionMapMap(tree.root.data) != null)
                    val solutionMap = solutionMapMap(tree.root.data)
                    tree.root.traversePreOrder(tree.root.data)((z,a) => {
                                               if(z != a){
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

  def testGraphCLRS_Chap22_v = {
    //test with the second vertex ... u
    val (graph,vv,solutionMapMap) = dfsGraphCLRS_chap22_v
    var trees = new DFS().dftree(graph,vv)

    assert(trees.length == 3)
    trees.foreach(tree => {
                    assert(solutionMapMap(tree.root.data) != null)
                    val solutionMap = solutionMapMap(tree.root.data)
                    tree.root.traversePreOrder(tree.root.data)((z,a) => {
                                               if(z != a){
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
  

  def dfsGraphCLRS_chap22(): (Graph[char],HashMap[Vertex[char],HashMap[Vertex[char],List[Edge[char]]]]) = {
    val vu = Vertex[char]('u');
    val vv = Vertex[char]('v');
    val vw = Vertex[char]('w');
    val vx = Vertex[char]('x');
    val vy = Vertex[char]('y');
    val vz = Vertex[char]('z');

	val vertices:List[Vertex[char]] = vu :: vv :: vw :: vx :: vy :: vz :: Nil 
 
    val graph:Graph[char] = Graph[char](vertices,true) 
    graph.addEdge(vu,vv)
    graph.addEdge(vu,vx)
    graph.addEdge(vx,vv)
    graph.addEdge(vv,vy)
    graph.addEdge(vy,vx)
    graph.addEdge(vw,vy)
    graph.addEdge(vw,vz)
    graph.addEdge(vz,vz)
    
    
    val solutionMap = new HashMap[Vertex[char],HashMap[Vertex[char],List[Edge[char]]]]()

    val uMap = new HashMap[Vertex[char],List[Edge[char]]]

    uMap += vu -> (graph.getEdge(vu,vv).get :: Nil)
    uMap += vv -> (graph.getEdge(vv,vy).get ::Nil)
    uMap += vy -> (graph.getEdge(vy,vx).get :: Nil)

    val wMap = new HashMap[Vertex[char],List[Edge[char]]]
    wMap += vw -> (graph.getEdge(vw,vz).get :: Nil)
    
    solutionMap += vu -> uMap
    solutionMap += vw -> wMap
    
    (graph,solutionMap)
    
  }

  def dfsGraphCLRS_chap22_v(): (Graph[char],Vertex[char],HashMap[Vertex[char],HashMap[Vertex[char],List[Edge[char]]]]) = {
    val vu = Vertex[char]('u');
    val vv = Vertex[char]('v');
    val vw = Vertex[char]('w');
    val vx = Vertex[char]('x');
    val vy = Vertex[char]('y');
    val vz = Vertex[char]('z');

	val vertices:List[Vertex[char]] = vu :: vv :: vw :: vx :: vy :: vz :: Nil 
 
    val graph:Graph[char] = Graph[char](vertices,true) 
    graph.addEdge(vu,vv)
    graph.addEdge(vu,vx)
    graph.addEdge(vx,vv)
    graph.addEdge(vv,vy)
    graph.addEdge(vy,vx)
    graph.addEdge(vw,vy)
    graph.addEdge(vw,vz)
    graph.addEdge(vz,vz)
    
    
    val solutionMap = new HashMap[Vertex[char],HashMap[Vertex[char],List[Edge[char]]]]()

    val uMap = new HashMap[Vertex[char],List[Edge[char]]]

    val vMap = new HashMap[Vertex[char],List[Edge[char]]]

    vMap += vv -> (graph.getEdge(vv,vy).get ::Nil)
    vMap += vy -> (graph.getEdge(vy,vx).get :: Nil)

    val wMap = new HashMap[Vertex[char],List[Edge[char]]]
    wMap += vw -> (graph.getEdge(vw,vz).get :: Nil)
    
    solutionMap += vu -> uMap
    solutionMap += vv -> vMap
    solutionMap += vw -> wMap
    
    (graph,vv,solutionMap)
    
  }


}
