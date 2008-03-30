package net.pragyah.scalgorithms.aima.search.csp

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.{Map,Queue}
import scala.collection.immutable.Set
import scala.collection.jcl.TreeMap

/*
 * Test the CSP with MRV+DegreeHeuristic and LeastConstrainingValue heuristic 
 * by throwing in a new heuristic ... which is ForwardCheckingHeuristic
 * This heuristic would be pass-on as a post-assignment heuristic that would go ahead
 * and remove those values from the adjoining variables' domain that may violate the constraints
 * given the current assignment
 */
class TestCSP_ValVarFwdChkHeuristic extends TestCSP_VarAndValHeuristic{

  /*
   * Get all the adjoining variables and remove the current assignment Color from
   * their domain list
   */
  def forwardCheckingHeuristic(domain:Domain[String,String],x:String,v:String,assignment:Assignment[String,String]):Domain[String,String] = {
    val adjs = adjMatrix(x)
    var d = domain
    adjs.foreach(adj => d =  d - (adj,v))
    d
  }

    //TEST
  override def testMapColoring = {
    
    // TEST THE HEURISTIC HERE
    val csp = new CSP[String,String](vars,constraints,domain)
    
    def varSelectHeuristic = csp.mostConstrainedVariableHeuristic(degreeHeuristic)_

    
    val assignmentOpt = csp.backtrackingSearch(varSelectHeuristic,leastConstrainingValueHeuristic _,null,forwardCheckingHeuristic _)
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
