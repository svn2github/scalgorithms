package net.pragyah.scalgorithms.aima.logic.propositional.inference

import scala.collection.mutable.HashMap

class ForwardChaining extends InferenceAlgorithm[HornClause,Symbol]{
  
	override def entails(kb:KnowledgeBase[HornClause],q:Symbol) : boolean = {
	   //KB , the knowledge base, a set of propositional Horn clauses 	
       // q , the query, a proposition symbol
	  
	  //a table, indexed by clause, initially the number of premises
     //this keeps track of how many premises of each implication are as yet unknown. 
     // whenever a new symbol p from the agenda is processed, the count is reduced by one for
     /// each implication in whose premise p appears.
     // if a count reaches zero, all the premises of the implication are known so its conclusion can be added to the agenda.
	  val count = new HashMap[HornClause,int]();
      kb.sentences.foreach(s=>{count(s) = s.body.size})
   
   
      // table, indexed by symbol, each entry initialy false
      // this keeps track of which symbols have been processed. an inferred symbol need not be added to the agenda
        // if it has been processed previously. This avoids redundant work. it also prevents infinite loops that could be 
        // caused by implications such as P => Q and Q => P
      val inferred = new HashMap[Symbol,boolean]();
      kb.sentences.foreach(_.body.foreach(inferred(_) = false))
      
      // a list of symbols, initially the symbols known to be true in KB
      //this keeps track of symbols known to be true but not yet "processed"
      var agenda = List[Symbol]();
      kb.trueSymbols.foreach(s =>{agenda = s :: agenda}) 

      //go till the agenda has something 
      while(!agenda.isEmpty){
        val p = agenda.head; agenda = agenda.tail; // p = POP(agenda)
        //do the stuff only if p has not been infered already
        if(!inferred(p)){
          inferred(p) = true
          count.filterKeys(_.body.contains(p)).foreach(clauseNum => {
                  val clause = clauseNum._1
                  val cnt = clauseNum._2
                  count(clause) = cnt -1
                  if(count(clause) == 0){
                    if(clause.head == q) return true
                    agenda = agenda ::: List(clause.head)
                  }
             }
          )
        }
        
      }
      
	  false
	}
  
}