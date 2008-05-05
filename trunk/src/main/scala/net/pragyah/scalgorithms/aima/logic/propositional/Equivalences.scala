package net.pragyah.scalgorithms.aima.logic.propositional

import net.pragyah.scalgorithms.aima.logic.propositional.{!,ComplexSentence,BinarySentence}
import net.pragyah.scalgorithms.aima.logic.propositional.Operator.{A,V,->,<->}

object Equivalences {
   def applyDoubleNegationElimination(sentence: ![Sentence]):Sentence = {
     assume(sentence.sentence.isInstanceOf[![Sentence]])
     sentence.sentence.asInstanceOf[![Sentence]].sentence
     
   }
 
   
  def applyContraposition(sentence:BinarySentence[Sentence,Sentence]):BinarySentence[Sentence,Sentence] = {
    assume(sentence.op == ->)
    BinarySentence[Sentence,Sentence](->,!sentence.right,!sentence.left)
    
  }
 
  def applyImplicationElimination(sentence:BinarySentence[Sentence,Sentence]):BinarySentence[Sentence,Sentence] = {
    assume(sentence.op == ->)
    //(a -> b) == !a V b
    return BinarySentence[Sentence,Sentence](V,!sentence.left,sentence.right)
  }
  
  
  def applyBiconditionalElimination(sentence:BinarySentence[Sentence,Sentence]):BinarySentence[Sentence,Sentence] = {
    
    assume(sentence.op == <->)
    // (a <-> b) == ((a -> b) A (b -> a))
    val s1 = BinarySentence[Sentence,Sentence](->,sentence.left,sentence.right)
    val s2 = BinarySentence[Sentence,Sentence](->,sentence.right,sentence.left)
    return BinarySentence[Sentence,Sentence](A,s1,s2)
  }
  
  
  def applyDeMorgan(sentence: ![ComplexSentence[Sentence]]):ComplexSentence[Sentence] = {
    assume(sentence.sentence.isInstanceOf[BinarySentence[Sentence,Sentence]] || sentence.sentence.isInstanceOf[MultiSentence[Sentence]])
    assume(sentence.sentence.op == A || sentence.sentence.op == V)
    
    sentence.sentence.op match{
           // !(a A b) == !a V !b
		case A => {
		  if(sentence.sentence.sentences.size == 2)
             return BinarySentence[Sentence,Sentence](V,!sentence.sentence.sentences.head,!sentence.sentence.sentences.tail.head)
          else
             return MultiSentence[Sentence](V,sentence.sentence.sentences.foldRight(List[Sentence]())((s,l) =>{ l:::List(!s) }))
		}
           // !(a V b) == !a A !b
		case V => {
		  if(sentence.sentence.sentences.size == 2)
             return BinarySentence[Sentence,Sentence](A,!sentence.sentence.sentences.head,!sentence.sentence.sentences.tail.head)
          else
             return MultiSentence[Sentence](A,sentence.sentence.sentences.foldRight(List[Sentence]())((s,l) =>{ l:::List(!s) }))
		}
    }
  }
  
  
  def applyDistributivityAoverV(sentence:ComplexSentence[Sentence]):MultiSentence[BinarySentence[Sentence,Sentence]] = {
    var s:BinarySentence[Sentence,Sentence] = null
    assume(sentence.isInstanceOf[BinarySentence[Sentence,Sentence]] || sentence.isInstanceOf[MultiSentence[Sentence]])
    assume(sentence.op == A)

    if(sentence.isInstanceOf[MultiSentence[Sentence]]){
      assume(sentence.sentences.size == 2)
      s = BinarySentence[Sentence,Sentence](sentence.op,sentence.sentences.head,sentence.sentences.tail.head)
    }else{
      s = sentence.asInstanceOf[BinarySentence[Sentence,Sentence]]
    }
    
    var notToBreak:Sentence = null
    var toBreak:ComplexSentence[Sentence] = null
    
    if (s.right.isInstanceOf[AtomicSentence] || s.right.isInstanceOf[![Sentence]]){
      assume(s.left.isInstanceOf[ComplexSentence[Sentence]])
      assume(s.left.asInstanceOf[ComplexSentence[Sentence]].op == V)
      toBreak = s.left.asInstanceOf[ComplexSentence[Sentence]]
      notToBreak = s.right
    }
    else {
      assume(s.right.isInstanceOf[ComplexSentence[Sentence]])
      assume(s.right.asInstanceOf[ComplexSentence[Sentence]].op == V)
      toBreak = s.right.asInstanceOf[ComplexSentence[Sentence]]
      notToBreak = s.left
    }
    
    val distAV:MultiSentence[BinarySentence[Sentence,Sentence]] = null
    
    var sentences = List[BinarySentence[Sentence,Sentence]]()
    toBreak.sentences.foreach( sentence => {
                                 sentences = sentences ::: List(BinarySentence[Sentence,Sentence](A,notToBreak,sentence)) 
                               })
    

    
    return MultiSentence[BinarySentence[Sentence,Sentence]](V,sentences)
  }
  
  def applyDistributivityVoverA(sentence:ComplexSentence[Sentence]):MultiSentence[BinarySentence[Sentence,Sentence]] = {
    var s:BinarySentence[Sentence,Sentence] = null
    assume(sentence.isInstanceOf[BinarySentence[Sentence,Sentence]] || sentence.isInstanceOf[MultiSentence[Sentence]])
    assume(sentence.op == V)

    if(sentence.isInstanceOf[MultiSentence[Sentence]]){
      assume(sentence.sentences.size == 2)
      s = BinarySentence[Sentence,Sentence](sentence.op,sentence.sentences.head,sentence.sentences.tail.head)
    }else{
      s = sentence.asInstanceOf[BinarySentence[Sentence,Sentence]]
    }
    
    var notToBreak:Sentence = null
    var toBreak:ComplexSentence[Sentence] = null
    
    if (s.right.isInstanceOf[AtomicSentence] || s.right.isInstanceOf[![Sentence]]){
      assume(s.left.isInstanceOf[ComplexSentence[Sentence]])
      assume(s.left.asInstanceOf[ComplexSentence[Sentence]].op == A)
      toBreak = s.left.asInstanceOf[ComplexSentence[Sentence]]
      notToBreak = s.right
    }
    else {
      assume(s.right.isInstanceOf[ComplexSentence[Sentence]])
      assume(s.right.asInstanceOf[ComplexSentence[Sentence]].op == A)
      toBreak = s.right.asInstanceOf[ComplexSentence[Sentence]]
      notToBreak = s.left
    }
    
    val distAV:MultiSentence[BinarySentence[Sentence,Sentence]] = null
    
    var sentences = List[BinarySentence[Sentence,Sentence]]()
    toBreak.sentences.foreach( sentence => {
                                 sentences = sentences ::: List(BinarySentence[Sentence,Sentence](V,notToBreak,sentence)) 
                               })
    

    
    return MultiSentence[BinarySentence[Sentence,Sentence]](A,sentences)
  }
  
}
