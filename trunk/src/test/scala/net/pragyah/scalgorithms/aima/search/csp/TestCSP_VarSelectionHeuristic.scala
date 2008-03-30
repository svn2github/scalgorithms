package net.pragyah.scalgorithms.aima.search.csp

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.{Map,Queue}
import scala.collection.immutable.Set
import scala.collection.jcl.TreeMap

/*
 * Test CSP with Variable selection Heuristic
 * Uses the MRV heuristic provided by CSP by feeding with a tie-breaker DegreeHeuristic for this problem
 * 
 */
class TestCSP_VarSelectionHeuristic  extends TestCSP_MapColoring{
  
  /*
   * The tie breaker heuristic which would be fed into MRV heuristic
   * Whenenever there is a tie for the most-constrainted variable spot in the domain .. 
   * this heuristic would be invoked to determine which of the tied variables is participating
   * in most constraints thus reducing the branching factor for the future.
   */
     def degreeHeuristic(mcvs:List[String],assignment:Assignment[String,String],constraints:List[Constraint[String,String]]) : String = {
      val unassigned = assignment.unassigned
      var tm = new TreeMap[int,List[String]]()

      mcvs.foreach(v => {
                      var count = adjMatrix(v).filter(unassigned.contains(_)).size //not a very efficient way though
                      if(!tm.contains(count)) tm += count -> List(v) else tm += count -> (v :: tm(count))
                   }
                   
      )
      //if you have a tie again .. then just return any of them ... or else you have a single one with the highest degree
      tm(tm.keySet.toList.reverse.head).head
    }
  
  override def testMapColoring = {
    

    val csp = new CSP[String,String](vars,constraints,domain)
    //feed in MRV with degree heuristic
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
