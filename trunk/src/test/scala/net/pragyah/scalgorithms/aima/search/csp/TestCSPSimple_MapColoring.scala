package net.pragyah.scalgorithms.aima.search.csp

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.{Set,Map}

class TestCSPSimple_MapColoring  extends TestCase{
  
  val WA = "WA"
  val NT = "NT"
  val SA = "SA"
  val Q = "Q"
  val NSW = "NSW"
  val V = "V"
  val T = "T"

  val vars = WA :: NT :: SA :: Q :: NSW :: V :: T :: Nil
  val vals = Set[String]()
  vals += "RED"
  vals += "BLUE"
  vals += "GREEN"
  
  val adjMatrix = Map[String,List[String]]()
  val waAdj = NT::SA::Nil
  adjMatrix += WA -> waAdj
  val ntAdj = WA::SA::Q::Nil
  adjMatrix += NT -> ntAdj
  val saAdj = WA::NT::Q::NSW::V::Nil
  adjMatrix += SA -> saAdj
  val qAdj = SA::NT::NSW::Nil
  adjMatrix += Q -> qAdj
  val nswAdj = SA::Q::V::Nil
  adjMatrix += NSW -> nswAdj
  val vAdj = SA::NSW::Nil
  adjMatrix += V -> vAdj
  
  adjMatrix += T -> Nil
  
  
  
  val domain = Domain[String,String](vars)
  vars.foreach(domain += (_,vals))
  
  def constraints:List[Constraint[String,String]] = {
    new MapConstraint() :: Nil
  }

  class MapConstraint extends Constraint[String,String]  {
    def satisfied(assignment:Assignment[String,String],x:String,v:String) : boolean = {
      val adjs = adjMatrix(x)
      !adjs.exists(adj => {
        assignment(adj) match{
        case None => false
        case opt => opt.get == v
        }
      })
    }
  }

  def testMapColoring = {
    
    val csp = new CSP[String,String](vars,constraints,domain)
    val assignment = csp.backtrackingSearch
    assertNotNull(assignment)
    assert(assignment != None)
    assertTrue(assignment.get.complete)
    assert(assignment.get.vars == vars)
    
    println(assignment.get)
    
    
  }
}
