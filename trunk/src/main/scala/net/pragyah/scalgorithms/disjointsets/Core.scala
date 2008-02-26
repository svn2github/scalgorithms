package net.pragyah.scalgorithms.disjointsets

trait DisjointSet[A] {

  def += (x:A) = makeSet(x)
  def apply(x:A):A = findSet(x)

  def makeSet(x:A)
  def findSet(x:A):A
  def union(x:A,y:A)

}
