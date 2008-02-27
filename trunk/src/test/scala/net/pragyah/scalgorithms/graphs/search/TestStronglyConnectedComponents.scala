package net.pragyah.scalgorithms.graphs.search

import scala.collection.mutable.HashMap

import junit.framework.TestCase
import junit.framework.Assert._

class TestStronglyConnectedComponents extends TestCase{
  
   def testStronglyConnectedComponents = {
        
    val (graph,component) =  sccGraphCLRS_Chap22
    val trees = new DFS().getStronglyConnectedComponents(graph)
    
    assert(trees.length == 4)
    trees.foreach(tree => {
                    tree.root.traversePreOrder(tree.root.data)((z,a) => {
                                               if(z != a){
                                                 val (edgeTo,edgeFro) = (graph.getEdge(z,a),graph.getEdge(a,z))
                                                 assert(edgeTo != null || edgeFro != null, "No edge in between "+z.data+" and "+a.data +" in the graph") // verify that such an edge exists
                                                 assert(component(z) == component(a))
                                                 component(z)
                                               }
                                               a
                                          })
                  })
   
   }
    
   def sccGraphCLRS_Chap22 : (Graph[char],HashMap[Vertex[char],Int]) = {
    
    val va =  Vertex[char]('a');
    val vb =  Vertex[char]('b');
    val vc =  Vertex[char]('c');
    val vd =  Vertex[char]('d');
    val ve =  Vertex[char]('e');
    val vf =  Vertex[char]('f');
    val vg =  Vertex[char]('g');
    val vh =  Vertex[char]('h');
    
    val vertices:List[Vertex[char]] = va :: vb :: vc :: vd :: ve :: vf :: vg :: vh  :: Nil 
    val graph:Graph[char] = Graph(vertices,true)
    
    graph.addEdge(va,vb)
    graph.addEdge(vb,vc)
    graph.addEdge(vb,ve)
    graph.addEdge(vb,vf)
    graph.addEdge(vc,vd)
    graph.addEdge(vc,vg)
    graph.addEdge(vd,vc)
    graph.addEdge(vd,vh)
    graph.addEdge(ve,va)
    graph.addEdge(ve,vf)
    graph.addEdge(vf,vg)
    graph.addEdge(vg,vf)
    graph.addEdge(vg,vh)
    graph.addEdge(vh,vh)
    
      
    val solutionMap = new HashMap[Vertex[char],Int]()
    solutionMap += va -> 1
    solutionMap += vb -> 1
    solutionMap += ve -> 1

    solutionMap += vc  -> 2
    solutionMap += vd -> 2

    solutionMap += vf  -> 3
    solutionMap += vg -> 3

    solutionMap += vh -> 4

    (graph,solutionMap)

  
  }

  
    
    
    
}
