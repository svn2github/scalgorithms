package net.pragyah.scalgorithms.aima.search.csp

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.{Set,Map}
import scala.collection.jcl.TreeMap

class TestCSP_MapColoring  extends TestCase{
  
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

  def testMapColoring_NoHeuristic = {
    
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
  
  
  def testMapColoring_VariableSelectionHeuristic = {
    
    def degreeHeuristic(mcvs:List[String],assignment:Assignment[String,String],constraints:List[Constraint[String,String]]) : String = {
      val unassigned = assignment.unassigned
      var tm = new TreeMap[int,List[String]]()

      mcvs.foreach(v => {
                      var count = adjMatrix(v).filter(unassigned.contains(_)).size //not a very efficient way though
                      if(!tm.contains(count)) tm += count -> List(v) else tm += count -> (v :: tm(count))
                   }
                   
      )
      tm(tm.keySet.toList.reverse.head).head
    }

    val csp = new CSP[String,String](vars,constraints,domain)
    def varSelectHeuristic = csp.mostConstrainedVariableHeuristic(degreeHeuristic)_

    val assignmentOpt = csp.backtrackingSearch(varSelectHeuristic,null,null,null)
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
