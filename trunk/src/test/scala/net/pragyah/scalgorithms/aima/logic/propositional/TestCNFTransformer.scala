package net.pragyah.scalgorithms.aima.logic.propositional

import net.pragyah.scalgorithms.aima.logic.propositional.Operator.{A,V,->,<->}


import junit.framework.TestCase
import junit.framework.Assert._

class TestCNFTransformer extends TestCase{
  
  def testTransformerSimple = {
    
    val B11 = Symbol("B11")
    val P12 = Symbol("P12")
    val P21 = Symbol("P21")
    
    val rt = BinarySentence[Symbol,Symbol](V,P12,P21)
    val raw = BinarySentence[Symbol,Sentence](<->,B11,rt)
    val cnf = new CNFTransformer(raw).process
    assert(cnf.isInstanceOf[MultiSentence[Sentence]])
    assert(cnf.asInstanceOf[MultiSentence[Sentence]].op == A)
    assert(cnf.asInstanceOf[MultiSentence[Sentence]].sentences.size ==3)
    cnf.asInstanceOf[MultiSentence[Sentence]].sentences.foreach(s =>{
              assert(s.isInstanceOf[ComplexSentence[Sentence]]);
              val cs = s.asInstanceOf[ComplexSentence[Sentence]]
              assert(cs.op == V)
              assert(cs.sentences.size == 3 || cs.sentences.size == 2 )
              assert( cs.sentences.contains(!B11) &&cs.sentences.contains(P12) && cs.sentences.contains(P21) 
              ||cs.sentences.contains(B11) &&cs.sentences.contains(!P12)
              ||cs.sentences.contains(B11) &&cs.sentences.contains(!P21)
              )
              }
      )                          
                                                                 
    println(cnf)
    
    assert(true)
  } 
  
  
  def testNotAndNot = {
    val P = Symbol("P")
    val raw = !BinarySentence[Symbol,Sentence](A,P,!P)
    val cnf = new CNFTransformer(raw).process
    println(cnf)
    
  }
}