package net.pragyah.scalgorithms.graphs.transitiveClosure

trait TransitiveClosureAlgorithm[A] {

  def compute(G:Graph[A]) : Array[Array[boolean]] 

}
