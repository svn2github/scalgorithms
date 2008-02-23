package net.pragyah.scalgorithms.graphs

import scala.collection.mutable.{HashMap,Queue}

import net.pragyah.scalgorithms.trees.{Tree,Node}

object TreeGenerator {
  
  //generate trees for the given graph ... 
  def generateTrees[A](graph:Graph[A],parent:Vertex[A] => Option[Vertex[A]]) : List[Tree[Vertex[A]]] = {
    val nodes = new HashMap[Vertex[A],Node[Vertex[A]]]
    graph.vertices.foreach(v => nodes += v -> Node(v)) //create blank nodes for each vertex
    
    graph.vertices.foreach(v => {
                             val p = parent(v)
                             if(p != None){
                               nodes(v).setParent(nodes(p.get))
                               nodes(p.get).addChild(nodes(v))
                             }
                           })

    var trees = List[Tree[Vertex[A]]]()
    nodes.values.filter(_.parent == None).foreach( node => trees = new Tree(node) :: trees)
    trees
  }

  
  // generate treee using selected edges only
  def generateTrees[A](graph:Graph[A],edges:List[Edge[A]]) : List[Tree[Vertex[A]]] = {
    assume(!graph.directed," It's a directed graph .. i don't like it")

    var trees = List[Tree[Vertex[A]]]()
    
    val nodes = new HashMap[Vertex[A],Node[Vertex[A]]]
    graph.vertices.foreach(v => nodes += v -> Node(v))
    edges.foreach(_.highlighted =  true)
      
    
    
    val Q = new Queue[Vertex[A]]();
    Q += edges.head.v1
    
    while(!Q.isEmpty){
      var s = Q.dequeue
      val parentNode = nodes(s)
      s.edges.filter(_.highlighted).foreach(e => { val x = e.getOther(s)
                                             parentNode.addChild(nodes(x)); nodes(x).setParent(parentNode)
                                             Q += x 
                                             e.highlighted = false
                                             })
      }
    
    nodes.values.filter(_.parent == None).foreach( node => trees = new Tree(node) :: trees)
    trees
  }

  
}