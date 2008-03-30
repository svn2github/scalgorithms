package net.pragyah.scalgorithms.aima.search.csp

import scala.collection.mutable.{Map,HashMap}
import scala.collection.immutable.Set
import scala.compat.StringBuilder

object Domain{
  def apply[X,V](l:List[X]) = new Domain[X,V](l)
}

//immmutable
class Domain[X,V](val l:List[X],private val map:Map[X,Set[V]]) {
  
  def this(l:List[X]) = {
   this(l,HashMap[X,Set[V]]())
   l.foreach(map += _ -> Set[V]())
  }
  
  def apply(x:X) =  map(x)

  def + (x:X,v:V) = {
    val m = map.clone
    m(x) = m(x) + v
    new Domain(l,m)
  }
  
  def + (x:X,v:Set[V]) = {
    val m = map.clone
    m(x) = m(x) ++ v
    new Domain(l,m)
  }
  
  def - (x:X,v:V) = {
    val m = map.clone
    m(x) = m(x) - v
    new Domain(l,m)
  } 

  def hasEmpty = map.values.exists(_.size == 0)
  
  override def toString:String = {
    var sb = new StringBuilder()
    map.foreach(k => {
                 sb.append("\n").append(k._1).append(" -> ")
                 k._2.foreach(v => sb = sb.append(v).append(","))
                })
    sb.toString
  }
  
  override def clone() = {
    new Domain(l,map.clone)
  }
  
}
