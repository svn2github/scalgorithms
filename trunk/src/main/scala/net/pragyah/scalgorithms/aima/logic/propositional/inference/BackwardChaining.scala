package net.pragyah.scalgorithms.aima.logic.propositional.inference

import scala.collection.mutable.HashMap

class BackwardChaining  extends InferenceAlgorithm[HornClause,Symbol]{
  
	override def entails(kb:KnowledgeBase[HornClause],q:Symbol) : boolean = {
	  
	  val clauses = kb.sentences.filter(_.head == q)
   
            // table, indexed by symbol, each entry initialy false
      // this keeps track of which symbols have been processed. an inferred symbol need not be added to the agenda
        // if it has been processed previously. This avoids redundant work. it also prevents infinite loops that could be 
        // caused by implications such as P => Q and Q => P
      val inferred = new HashMap[Symbol,boolean]();
      kb.sentences.foreach(_.symbols.foreach(inferred(_) = false))
 
   
   
	  clauses.foreach(hc => {
	            
	                  hc.body.foreach(println
	                  )
	    
                   }
	  )
	  
	  false
	}

}
