package net.pragyah.scalgorithms.aima.search.csp

import scala.collection.jcl.TreeMap

class CSP[X,V](val vars:List[X],val cons:List[Constraint[X,V]],val domain:Domain[X,V]) {

  def this(vars:List[X],cons:List[Constraint[X,V]]) =this(vars,cons,Domain[X,V](vars))

  //dummy method - get any unassigned variable .. first in the list
  def simpleSelectUnassignedVariable(assignment:Assignment[X,V],domain:Domain[X,V]):X = {
    assignment.unassigned.head
  }
  //dummy method - get the list of ALL domain values for x
  def simpleOrderDomainValues(x:X,domain:Domain[X,V],assignment:Assignment[X,V]):List[V] = {
    domain(x).toList
  }
  //dummy method - return the domain ... unscathed  
  def simplePreprocessDomain(domain:Domain[X,V]):Domain[X,V] = {
    domain
  }
 
  //dummy method - return the domain ... unscathed
  def simplePostprocessDomain(domain:Domain[X,V],x:X,v:V,assignment:Assignment[X,V]):Domain[X,V] = {
    domain
  }
   
  def backtrackingSearch():Option[Assignment[X,V]] = backtrackingSearch(null,null,null,null)
  
  def backtrackingSearch(selectUnassignedVariable:(Assignment[X,V],Domain[X,V]) => X,
                            orderDomainValues:(X,Domain[X,V],Assignment[X,V]) => List[V],
                            preProcessDomain:Domain[X,V] => Domain[X,V],
                            postProcessDomain:(Domain[X,V],X,V,Assignment[X,V]) => Domain[X,V]) : Option[Assignment[X,V]] = {
    
                            //if any  of the above methods-arg is null ... assign the corresponding dummy method from above.
                            recursiveBacktrackingSearch(domain,
                                                         Assignment[X,V](vars),
                                                         if (selectUnassignedVariable != null) selectUnassignedVariable else simpleSelectUnassignedVariable _,
                                                         if (orderDomainValues != null) orderDomainValues else simpleOrderDomainValues _,
                                                         if (preProcessDomain != null) preProcessDomain else simplePreprocessDomain _,
                                                         if (postProcessDomain != null) postProcessDomain else simplePostprocessDomain _)
  }
  
  def recursiveBacktrackingSearch(domain:Domain[X,V],
                                  assignment:Assignment[X,V],
                                  selectUnassignedVariable:(Assignment[X,V],Domain[X,V]) => X,
                                  orderDomainValues:(X,Domain[X,V],Assignment[X,V]) => List[V],
                                  preProcessDomain:Domain[X,V] => Domain[X,V],
                                  postProcessDomain:(Domain[X,V],X,V,Assignment[X,V]) => Domain[X,V]) : Option[Assignment[X,V]] = {
    
    if(assignment.complete) return Some(assignment)
    //preprocss the domain .. to deweed it if possible .... 
    var preD = preProcessDomain(domain)
    //select the next varialble that should be assigned ... using the heuristic provided
    val v = selectUnassignedVariable(assignment,preD)
    //get the domain values of the current variable ordered in an effective fashion ... using the heuristic provided
    val vDomain = orderDomainValues(v,preD,assignment)
    
    vDomain.foreach(
      value => if(!cons.exists(!_.satisfied(assignment,v,value))){//if all the constraints are satisfied 
                     assignment += (v,value) 
                     // things may change for other variables with this assignment .. (forwardChecking can help here)
                     var postD = postProcessDomain(preD,v,value,assignment) 
                     if(postD.hasEmpty)  return None
                     val result =  recursiveBacktrackingSearch(postD,assignment,selectUnassignedVariable,orderDomainValues,preProcessDomain,postProcessDomain)
                     if(result == None) {
                       assignment -= v
                     }
                     else  return result
                  }
    )
   None 
  }
  
  /*
   * Minimum Remaining Values (MRV) OR Most Constrained Variable OR Fail-First heuristic.
   * Select the variable that has the least number of possible values remaining in its domain...
   * i,e, it is more likely to cause a failure soon ... 
   * This can be passed on as selectUnassignedVariable closure in recursiveBacktrackingSearch
   * 
   * If there are more than one such variables then take help of the problem-specific "degreeHeuristic" that should
   * return back a variable that is involved in the largest number of constraints on the other unassigned variables
   * 
   * complexity O(n + that-of-degree-heuristic)
   */
  def mostConstrainedVariableHeuristic(degreeHeuristic:(List[X],Assignment[X,V],List[Constraint[X,V]]) => X)(assignment:Assignment[X,V],domain:Domain[X,V]) :X = {
      val unassigned = assignment.unassigned
      var tm = new TreeMap[int,List[X]]()
      unassigned.foreach( v =>{
         val size = domain(v).size
         if(!tm.contains(size)) tm += size -> List(v) else tm += size -> (v :: tm(size))
        }
      )
      val  mcvs = tm(tm.keySet.firstKey)
      //if there is just one most-constraint variable
      if(mcvs.size == 1) return mcvs.head
      //if more than 1 .. then select the one with the least degree ... TIE BREAKER!!! 
      /* Attempt to reduce the the branching factor on future choices by selecting the variable that is inovlved in largets number of constraints
       * on other unassigned variables
       */
      else return degreeHeuristic(mcvs,assignment,cons)
      
    }
 
  

}