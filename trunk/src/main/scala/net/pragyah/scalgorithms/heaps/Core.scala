package net.pragyah.scalgorithms.heaps

trait Heap[A] {

  def insert(a:A)
  def extractMin() : Option[A]
  def delete(a:A)
  def minimum : Option[A]

}