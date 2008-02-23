package net.pragyah.scalgorithms.graphs.shortestpath.singlesource

import net.pragyah.scalgorithms.trees.Tree;

trait ShortestPathAlgo[A]{
  def getShortestPath(graph:Graph[A], s:Vertex[A]): Tree[Vertex[A]]
  
 
}