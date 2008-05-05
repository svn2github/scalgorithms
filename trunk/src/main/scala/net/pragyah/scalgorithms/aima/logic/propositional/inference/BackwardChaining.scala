package net.pragyah.scalgorithms.aima.logic.propositional.inference

import scala.collection.mutable.HashMap

class BackwardChaining  extends InferenceAlgorithm[HornClause,Symbol]{
  
	override def entails(kb:KnowledgeBase[HornClause],q:Symbol) : boolean = {
	  
	  
      val inferred = new HashMap[Symbol,boolean]();
      kb.sentences.foreach(_.symbols.foreach(inferred(_) = false))

	  kb.sentences.filter( _.head == q).foreach(hc => {
	    
	  })
	  
	  
	  false
	}

}
