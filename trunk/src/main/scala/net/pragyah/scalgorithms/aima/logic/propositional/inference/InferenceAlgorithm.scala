package net.pragyah.scalgorithms.aima.logic.propositional.inference

trait InferenceAlgorithm {
  
    def entails(kb:KnowledgeBase,alpha:Sentence) : boolean 

}
