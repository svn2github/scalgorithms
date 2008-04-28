package net.pragyah.scalgorithms.aima.logic.propositional.inference

import scala.collection.mutable.Set
import Operator.A

class Resolution extends InferenceAlgorithm{
  
    override def entails(kb:KnowledgeBase,alpha:Sentence) : boolean = {
      
      //clauses - the set of clauses in the CNF representation of KB A !alpha
      val clauses = getCNFClauses(BinarySentence(A,kb.asSentence, new !(alpha)))
      var _new = Set[Sentence]()
      
      while(true){
        pairs[Sentence](clauses.toList).foreach(pair => {
          //get all the resolvants resulting from each pair of sentences 
          val resolvents:Set[Sentence] = resolve(pair._1,pair._2)
          //if there is an empty clause in the resolventes ... this means that KB A !alpha is not satisfiable 
          //... which means that KB |= alpha
          if(resolvents.contains(EmptyClause())) return true
          //if there is no empty clause .. then carry on .. with collecting the resolvents for this pass in _new
          _new ++= resolvents;
        })
        // now .. if all the resolvents collected so far contain nothing new that is to be known ... which means 
        // that there is no further scope of getting an EmptyClause any further ... which means that KB A !alpha is 
        // satisfiable ... which means :) that KB |= alpha is not true afterall 
          if(_new.subsetOf(clauses))
            return false

          // if some new sentences/clauses are discovered... add them to the ResolutionClosure (clauses)
          clauses ++= _new
      }
      false

    }
    
    
    def resolve(Ci:Sentence,Cj:Sentence):Set[Sentence] = {
      return null
    }
    
    def getCNFClauses(sentence:Sentence):Set[Sentence] = {
      null
    }
    
    def  pairs[A](l:List[A]):Set[(A,A)] = {
      var _pairs:Set[(A,A)] = Set()
      if(l.size == 2) return _pairs + ((l(0),l(1)))
      l.tail.foldRight(l.head)((t,h) =>{_pairs += ((h,t));h})
      return _pairs ++ pairs(l.tail)
	}

}
