package net.pragyah.scalgorithms.aima.search.csp

import scala.collection.mutable.HashMap
import scala.compat.StringBuilder

object Assignment{
   def apply[X,Y](vars:List[X]) = new Assignment[X,Y](vars) 
}

class Assignment[X,V](val vars:List[X]) {
  val map = new HashMap[X,Option[V]]()
  vars.foreach(map += _ -> None)

  
  def apply(x:X) = map(x)
  def += (x:X,v:V) = map += x -> Some(v)
  def -= (x:X) = map(x) = None
  def complete = !map.values.exists(_ == None)
  def has(x:X)  = map(x) != None
  def unassigned = {
   val clone = map.clone
   clone.retain((a,b) => b == None)
   clone.keySet.toList
  }
  
  override def toString:String = {
    var sb = new StringBuilder()
    map.foreach(k => sb  = sb.append("\n").append(k._1).append(" -> ").append(k._2))
    sb.toString
  }
  

}
