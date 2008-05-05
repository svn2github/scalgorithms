package net.pragyah.scalgorithms.aima.logic.propositional

import net.pragyah.scalgorithms.aima.logic.propositional.Operator.{A,V,NOT,->,<->}

import scala.collection.mutable.HashSet

object KnowledgeBase{
 def apply[A <: Sentence]() = new KnowledgeBase[A]()
}

class KnowledgeBase[A <: Sentence]{
  
  var sentences = new HashSet[A]()
  private var trueList:List[Symbol] = Nil 
  private var falseList:List[Symbol] = Nil 
  
  def tell(sentence:A) =  sentences += sentence
  def += (sentence:A) =  sentences += sentence
  
  def tellTrue(symbol:Symbol) = trueList = symbol::trueList
  def tellFalse(symbol:Symbol) = falseList = symbol::falseList
  
  def asSentence:Sentence = {
    new MultiSentence(A)(sentences.toList);
  }
  
  def trueSymbols = trueList
  def falseSymbols = falseList
  
  override def toString = trueList.foldLeft[String](asSentence.toString + " "+Operator.A+" ")((str,symbol) => str+" "+Operator.A+" "+symbol+" = TRUE ")
  
}