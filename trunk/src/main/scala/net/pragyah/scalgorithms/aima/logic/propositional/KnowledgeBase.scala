package net.pragyah.scalgorithms.aima.logic.propositional

import net.pragyah.scalgorithms.aima.logic.propositional.Operator.{A,V,!,==>,<=>}

import scala.collection.mutable.HashSet

class KnowledgeBase{
  
  var sentences = new HashSet[Sentence]()
  
  def tell(sentence:Sentence) =  sentences += sentence
  def += (sentence:Sentence) =  sentences += sentence
  
  def asSentence:Sentence = {
    new MultiSentence(A,sentences.toList);
  }
}
