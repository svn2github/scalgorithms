package net.pragyah.scalgorithms.aima.search.csp

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.{Map,Queue}
import scala.collection.immutable.Set
import scala.collection.jcl.TreeMap
/*
 * Uses the MRV with degree heuristic but provides an additional heuristic to get the order 
 * in which the possible values (from the domain ) should be tried out on the selected variable.   
 */ 
class TestCSP_VarAndValHeuristic extends TestCSP_VarSelectionHeuristic{
  
 // NEED TO TEST THIS AS WELL ;)
  
  /*
   * prefer the value that rules out the fewest choices for
   * the  neighboring variable in the constraint graph
   * It tries to leave the maximum flexibility for subsequent variable assignments.
   * 
   */
  def leastConstrainingValueHeuristic(x:String,domain:Domain[String,String],assignment:Assignment[String,String]) = {
    val adjs = adjMatrix(x).filter(assignment.unassigned.contains(_)) // filter out all the already-assigned variables from the adjecency list
    val d = domain(x)
    var tm = new TreeMap[int,List[String]]()
    d.foreach(value => {
                 var count = 0;
                 adjs.foreach( adj =>{ // find out the number of adjoining variables affected by this potential assignment (of the 'value/color')
                   // if this color is assigned to the varibale .. the adjoining variables cannot have the same color ... 
                                  if(domain(adj).contains(value)) count += 1
                               }
                 )
                 if(!tm.contains(count)) tm += count -> List(value) else tm += count -> (value :: tm(count))
              }
    )
    
    val keys = tm.keySet.toList.reverse
    //consolidate the list of values in the ascending order of 'count'
    var lst:List[String] = Nil 
    keys.foreach(key => lst = tm(key) ::: lst)
    lst
  }

  override def testMapColoring = {
    
    // TEST THE HEURISTIC HERE

    val csp = new CSP[String,String](vars,constraints,domain)
    def varSelectHeuristic = csp.mostConstrainedVariableHeuristic(degreeHeuristic)_

    
    val assignmentOpt = csp.backtrackingSearch(varSelectHeuristic,leastConstrainingValueHeuristic _,null,null)
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
