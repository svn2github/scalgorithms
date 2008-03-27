package net.pragyah.scalgorithms.aima.search.csp

import scala.collection.mutable.{Set,Map,HashMap}
import scala.compat.StringBuilder

object Domain{
  def apply[X,V](l:List[X]) = new Domain[X,V](l)
}

class Domain[X,V](val l:List[X]) {
  
  private val map:Map[X,Set[V]] = HashMap[X,Set[V]]();
  l.foreach(map += _ -> Set[V]())
  
  def apply(x:X) =  map(x)

  def += (x:X,v:V) = map(x) += v
  
  def += (x:X,v:Set[V]) = map(x) ++= v
  
  def -= (x:X,v:V) = map(x) -= v 

  def hasEmpty = map.values.exists(_.size == 0)
  
  override def toString:String = {
    var sb = new StringBuilder()
    map.foreach(k => {
                 sb.append("\n").append(k._1).append(" -> ")
                 k._2.foreach(v => sb = sb.append(v).append(","))
                })
    sb.toString
  }
  
}
