package net.pragyah.scalgorithms.graphs.mst

import net.pragyah.scalgorithms.trees.Tree

trait MSTAlgo[A]{
  def generateMST(graph:Graph[A]): Tree[Vertex[A]]
}