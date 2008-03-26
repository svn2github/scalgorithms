package net.pragyah.scalgorithms.aima.search.csp

class CSP[X,V](val vars:List[X],val cons:List[Constraint[X,V]],val domain:Domain[X,V]) {

  def this(vars:List[X],cons:List[Constraint[X,V]]) =this(vars,cons,new Domain[X,V](vars))

  //get any unassigned variable
  def simpleSelectUnassignedVariable(assignment:Assignment[X,V]):X = {
    assignment.unassigned.head
  }
  //get the list of ALL domain values for x
  def simpleOrderDomainValues(x:X,domain:Domain[X,V],assignment:Assignment[X,V]):List[V] = {
    domain(x).toList
  }
  //get the list of ALL domain values for x
  def simplePreprocessDomain(domain:Domain[X,V]):Domain[X,V] = {
    domain
  }
 
  //get the list of ALL domain values for x
  def simplePostprocessDomain(domain:Domain[X,V],x:X,v:V,assignment:Assignment[X,V]):Domain[X,V] = {
    domain
  }
   
  def backtrackingSearch() = recursiveBacktrackingSearch(domain,
                                                         Assignment[X,V](vars),
                                                         simpleSelectUnassignedVariable _,
                                                         simpleOrderDomainValues _,
                                                         simplePreprocessDomain _,
                                                         simplePostprocessDomain _)
  
  def recursiveBacktrackingSearch(domain:Domain[X,V],
                                  assignment:Assignment[X,V],
                                  selectUnassignedVariable:Assignment[X,V] => X,
                                  orderDomainValues:(X,Domain[X,V],Assignment[X,V]) => List[V],
                                  preProcessDomain:Domain[X,V] => Domain[X,V],
                                  postProcessDomain:(Domain[X,V],X,V,Assignment[X,V]) => Domain[X,V]) : Option[Assignment[X,V]] = {
    
    if(assignment.complete) return Some(assignment)
    var d = preProcessDomain(domain)
    val v = selectUnassignedVariable(assignment)
    val vDomain = orderDomainValues(v,d,assignment)
    
    vDomain.foreach(
      value => if(!cons.exists(!_.satisfied(assignment,v,value))){
                     assignment += (v,value)
                     d = postProcessDomain(d,v,value,assignment)
                     if(d.hasEmpty)  return None
                     val result =  recursiveBacktrackingSearch(d,assignment,selectUnassignedVariable,orderDomainValues,preProcessDomain,postProcessDomain)
                     if(result == None) {
                       assignment -= v
                     }
                     else  return result
                  }
    )
   None 
  }
  
  


}
