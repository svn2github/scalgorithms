package net.pragyah.scalgorithms.aima.logic.propositional.inference
                        
trait InferenceAlgorithm[A <: Sentence,B<:Sentence] {
  
    def entails(kb:KnowledgeBase[A],alpha:B) : boolean 

}
