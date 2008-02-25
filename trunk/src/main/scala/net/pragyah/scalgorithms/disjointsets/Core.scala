package net.pragyah.scalgorithms.disjointsets

trait DisjointSet[A] {

  def makeSet(x:A)
  def findSet(x:A):A
  def union(x:A,y:A)

}
