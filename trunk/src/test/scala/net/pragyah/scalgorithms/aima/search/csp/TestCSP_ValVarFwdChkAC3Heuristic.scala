package net.pragyah.scalgorithms.aima.search.csp

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.{Map,Queue}
import scala.collection.immutable.Set
import scala.collection.jcl.TreeMap

/*
 * This one makes the use of all the other heuristics (MRV+DegreeHeuristic + LeastConstrainingValueHeuristic + ForwardCheck)
 * Along with a strategy for constraint propagation - AC3
 * ForwardCheck heuristic does not detect all the inconsistency because it does not look far enough ahead ... 
 * in that removal of values from the domains of two adjoining variables (of the current variable) may violate 
 * the constraints in which they participate with their adjoining variables respectively.
 * AC3 propagets the constraints further
 * This would be applied as a preprocessing step ... wherein ... after ForwardChecking is done and before new variable is to be selected for assignment
 * 
 */


class TestCSP_ValVarFwdChkAC3Heuristic extends TestCSP_ValVarFwdChkHeuristic {
 
  /*
   * shake-shake-shake the domain to filterout the useless
   * values for each variable.
   * 
   */
  def ac3(domain:Domain[String,String]):Domain[String,String] = {
    var d = domain
    val q = new Queue[(String,String)]();
    //add all possible pairs of variables (edges in a constraint graph ... splitting undirected edges to two directed edges
    adjMatrix.keys.foreach( xi => {
                              adjMatrix(xi).foreach(q += (xi,_))
                            })
    
    while(!q.isEmpty){  
      val x = q.dequeue;// dequeu each edge and see if there is any inconsistancy in the first node/variable wrt to the second nod ...
      if(removeInconsistentValues(x._1,x._2)){ // if yes then 
        adjMatrix(x._1).foreach(q += (x._1,_)) // add this first-node with all the possible edges comming into it to the end of the queue ... 	
                                               // because a removal from its domain may spoil its relations with its adjoining variables
      }
    }
    
    def removeInconsistentValues(xi:String,xj:String):boolean = {
      var removed = false
      d(xi).foreach(vi => 
                             if(d(xj).filter(_ != vi).size == 0){ // check if there exists a color in the adjoining variable compatible with this color (vi) 
                               d = d - (xi,vi); 
                               removed = true
                             }
      )
      removed
    }
    d
  }
  

  
    //TEST
  override def testMapColoring = {
    
    // TEST THE HEURISTIC HERE
    val csp = new CSP[String,String](vars,constraints,domain)
    
    def varSelectHeuristic = csp.mostConstrainedVariableHeuristic(degreeHeuristic)_

    
    val assignmentOpt = csp.backtrackingSearch(varSelectHeuristic,leastConstrainingValueHeuristic _,ac3 _,forwardCheckingHeuristic _)
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
