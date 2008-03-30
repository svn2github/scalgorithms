package net.pragyah.scalgorithms.aima.search.csp

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.{Map,Queue}
import scala.collection.immutable.Set
import scala.collection.jcl.TreeMap

/*
 * Test CSP with basic map coloring with no (or minimal) heuristics 
 */

class TestCSP_MapColoring  extends TestCase{
  
  val WA = "WA"
  val NT = "NT"
  val SA = "SA"
  val Q = "Q"
  val NSW = "NSW"
  val V = "V"
  val T = "T"

  val vars = WA :: NT :: SA :: Q :: NSW :: V :: T :: Nil
  var vals = Set[String]()
  vals = vals + "RED"
  vals = vals + "BLUE"
  vals = vals + "GREEN"
  
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
  
  
  
  var domain = Domain[String,String](vars)
  vars.foreach(v => domain = domain + (v,vals))
  
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
    val assignmentOpt = csp.backtrackingSearch
    
    assertNotNull(assignmentOpt)
    assert(assignmentOpt != None)
    val assignment = assignmentOpt.get
    assertTrue(assignment.complete)
    assert(assignment.vars == vars)
    vars.foreach(x => {
                   val v = assignment(x).get
                   constraints.foreach(c => assert(c.satisfied(assignment,x,v)))
                 })
    println(assignment)
  }
 
}
