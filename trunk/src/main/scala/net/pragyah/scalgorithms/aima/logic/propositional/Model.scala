package net.pragyah.scalgorithms.aima.logic.propositional

import scala.collection.mutable.HashMap

object Model{
  def apply() = new Model()
}
class Model {
  private val map = HashMap[Symbol,Option[TruthValue]]()
  
  def + (s:Symbol):Model = {
    val model = new Model();
    model.map ++= map
    model.map += s -> None
    model
  }
  
  def + (s:Symbol,v:TruthValue):Model = {
    val model = new Model();
    model.map ++= map
    model.map += s -> Some(v)
    model
  }

  def apply(s:Symbol) = map(s)

  def isTrue(sentence : Sentence) : boolean = {
    //TODO
    false
  }

}
