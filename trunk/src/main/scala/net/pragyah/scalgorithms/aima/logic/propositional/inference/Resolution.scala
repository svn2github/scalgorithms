package net.pragyah.scalgorithms.aima.logic.propositional.inference

import scala.collection.immutable.Set
import net.pragyah.scalgorithms.aima.logic.propositional.!
import Operator.{A,V}

class Resolution extends InferenceAlgorithm[Sentence,Sentence]{
  
    override def entails(kb:KnowledgeBase[Sentence],alpha:Sentence) : boolean = {
      
      //clauses - the set of clauses in the CNF representation of KB A !alpha
      var clauses = getCNFClauses(BinarySentence(A,kb.asSentence, !alpha))
      
      while(true){
        var _new = Set[Sentence]()
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
        println()
        clauses.foreach(s => _new = _new.excl(s))
        
        if(_new.size == 0)
            return false

          // if some new sentences/clauses are discovered... add them to the ResolutionClosure (clauses)
          
          clauses ++= _new
          
      }
      false

    }
    

    def resolve(Ci:Sentence,Cj:Sentence):Set[Sentence] = {
      
      var atomicSentences = scala.collection.immutable.Set[Sentence]() // Set ... for 'factoring'
      //if it's a complex sentence ... then get all the sentences out of it 

      Ci match {
	      case ns: ![Sentence] =>{
	        atomicSentences += ns
          }
	      case cs:ComplexSentence[AtomicSentence] =>{
	        assume(cs.op == Operator.V)
            if(cs.flatten == True())
              return Set() // we don't need to send back anything ... 
            else
              atomicSentences ++=  cs.sentences
            
	      }
	      case _ =>{// if it's an atomic sentence .. then just add it
	        atomicSentences += Ci 
	      }
      }
      
      Cj match {
	      case ns: ![Sentence] =>{
	        atomicSentences += ns
          }
	      case cs:ComplexSentence[AtomicSentence] =>{
	        assume(cs.op == Operator.V)
            if(cs.flatten == True())
              return Set() // we don't need to send back anything ... 
            else
              atomicSentences ++=  cs.sentences
	      }
	      case _ =>{// if it's an atomic sentence .. then just add it
	        atomicSentences += Cj 
	      }
      }
      
      var resolvents = Set[Sentence]()
      atomicSentences.filter( _.isInstanceOf[![Symbol]]).foreach(n =>{
        var l = List[Sentence]()
        atomicSentences.filter(s => s != n && s != n.symbols.head).foldLeft[List[Sentence]](l)((l1,s) => {l = s::l1;l})
        if(l.size != atomicSentences.size -1)
          {
		    if(l.size == 0)
		      resolvents += EmptyClause()
		    else if(l.size == 1)
		      resolvents += l.head
		    else
		      resolvents += MultiSentence[Sentence](V,l).flatten
          }
       }
      )
      return resolvents
    }
    
    def getCNFClauses(sentence:Sentence):Set[Sentence] = {
      val transformer = new CNFTransformer(sentence)
      val cnfSentence = transformer.process
      if(cnfSentence.isInstanceOf[ComplexSentence[Sentence]])
        return Set[Sentence]() ++ cnfSentence.asInstanceOf[ComplexSentence[Sentence]].sentences
      else{
        Set(cnfSentence)
      }
    }
    
    def  pairs[A](l:List[A]):Set[(A,A)] = {
      var _pairs:Set[(A,A)] = Set()
      if(l.size == 2) return _pairs + ((l(0),l(1)))
      l.tail.foldRight(l.head)((t,h) =>{_pairs += ((h,t));h})
      return _pairs ++ pairs(l.tail)
	}

}
