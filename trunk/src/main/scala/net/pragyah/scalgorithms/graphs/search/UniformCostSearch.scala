package net.pragyah.scalgorithms.graphs.search

import net.pragyah.scalgorithms.trees.Tree

class UniformCostSearch[A] {

  def search(graph:Graph[A],s:Vertex[A],goal:(Vertex[A]) => boolean) : List[Vertex[A]] = {
    assume(graph.vertices.contains(s))
    def h(v:Vertex[A]) = 0
    new A_Star().search(graph,s,goal,h)
  }

}
