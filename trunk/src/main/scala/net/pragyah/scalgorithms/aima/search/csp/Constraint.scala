package net.pragyah.scalgorithms.aima.search.csp

trait Constraint[X,V] {
  def satisfied(assignment:Assignment[X,V],x:X,v:V) : boolean

}
