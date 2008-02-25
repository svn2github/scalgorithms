package net.pragyah.scalgorithms.graphs.mst

import net.pragyah.scalgorithms.trees.{Tree,Node}

import scala.collection.mutable.{Set,HashSet}

import junit.framework.TestCase
import junit.framework.Assert._

class TestKruskalsAlgorithm extends TestCase{
  
  def testCLRS_Chap23_Graph = {
    val (graph,solutionEdges) =  mstGraphCLRS_chap23()
    
    var mstAlgo = new KruskalsAlgorithm[char]()
    var tree = mstAlgo.generateMST(graph)

    var totalMSTWeight:double = 0;

    tree.root.traversePreOrder[Vertex[char]](tree.root.data)( (z,a) => {
                                                               if(z != a){
                                                                 val edge = graph.getEdge(z,a)
                                                                 assert(edge  != None) // verify that such an edge exists
                                                                 assert(solutionEdges.contains(edge.get))
                                                                 totalMSTWeight = totalMSTWeight+edge.get.weight
                                                               }
                                                               a
                                                             }
    )
    assert(totalMSTWeight == 37)

  }
  
  def mstGraphCLRS_chap23(): (Graph[char],Set[Edge[char]]) = {
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

    var solutionEdges = new HashSet[Edge[char]]()
    solutionEdges += graph.getEdge(vg,vh).get
    
    solutionEdges += graph.getEdge(vh,va).get; 
    //OR .. since either can exist in the mst ..
    solutionEdges += graph.getEdge(vb,vc).get
    
    solutionEdges += graph.getEdge(va,vb).get
    solutionEdges += graph.getEdge(vg,vf).get
    solutionEdges += graph.getEdge(vf,vc).get
    solutionEdges += graph.getEdge(vc,vi).get
    solutionEdges += graph.getEdge(vc,vd).get
    solutionEdges += graph.getEdge(vd,ve).get
    solutionEdges += graph.getEdge(vb,vc).get

    (graph,solutionEdges)
    
  }

}
