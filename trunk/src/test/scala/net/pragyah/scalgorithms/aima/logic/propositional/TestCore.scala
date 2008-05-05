package net.pragyah.scalgorithms.aima.logic.propositional

import net.pragyah.scalgorithms.aima.logic.propositional.Operator.{A,V,->,<->}

import junit.framework.TestCase
import junit.framework.Assert._

import scala.collection.mutable.HashSet

class TestCore extends TestCase{
  
  def testTrue = {
    val sTrue = True();
    assert(sTrue.value)
    assert(sTrue.symbols == Nil)
  }

  def testFalse = {
    val sFalse = False();
    assertFalse(sFalse.value)
    assert(sFalse.symbols == Nil)
  }
  
  def testSymbol = {
    val symbol = Symbol("P")
    assert(symbol.name == "P")
    assert(symbol.symbols.head == symbol)
  }

  def testBinary = {
    
    val l = Symbol("L")
    val r = Symbol("R")

    val sentence = BinarySentence(V,l,r)
    assert(sentence.op == V)
    assert(sentence.sentences.length == 2)
    assert(sentence.sentences.contains(l))
    assert(sentence.sentences.contains(r))
    assert(sentence.left == l)
    assert(sentence.right == r)

    assert(sentence.symbols.length == 2)
    assert(sentence.symbols.contains(l))
    assert(sentence.symbols.contains(r))
    
  }

  def testMultipleWithSymbols = {
    val P = Symbol("P")
    val Q = Symbol("Q")
    val R = Symbol("R")
    val T = True()

    val sentence = MultiSentence(A,P::Q::R::T::Nil)
    assert(sentence.op == A)
    assert(sentence.sentences.length == 4 )
    assert(sentence.sentences.contains(P))
    assert(sentence.sentences.contains(Q))
    assert(sentence.sentences.contains(R))
    assert(sentence.sentences.contains(T))

	assert(sentence.symbols.length == 3)
    assert(sentence.symbols.contains(P))
    assert(sentence.symbols.contains(Q))
    assert(sentence.symbols.contains(R))
    
    
    

  }

  def testMultipleWithSentences = {
    val P = Symbol("P")
    val Q = Symbol("Q")
    val R = Symbol("R")
    val S = Symbol("S")
    val T = True()
    
    val sentence1 = BinarySentence(V,P,Q)
    val sentence2 = BinarySentence(V,R,S)
    
    
    val sentence = MultiSentence(A,sentence1::sentence2::T::Nil)
    assert(sentence.op == A)
    assert(sentence.sentences.length == 3 )
    assert(sentence.sentences.contains(sentence1))
    assert(sentence.sentences.contains(sentence2))
    assert(sentence.sentences.contains(T))
    
    val symbols = sentence.symbols
    assert(symbols.size == 4)
    assert(symbols.contains(P))
    assert(symbols.contains(Q))
    assert(symbols.contains(R))
    assert(symbols.contains(S))
    
  }
  
  def testMultipleWithMultipleSentences = {
    val P = Symbol("P")
    val Q = Symbol("Q")
    val R = Symbol("R")
    val S = Symbol("S")
    val T = True()
    
    val bSentence = BinarySentence(A,P,Q)
    
    val sentence1 = MultiSentence(V,bSentence::R::Nil)
    val sentence2 = MultiSentence(V,Q::R::S::Nil)
    val sentence3 = MultiSentence(V,R::S::T::Nil)
    
    
    val sentence = MultiSentence(A,sentence1::sentence2::sentence3::P::Q::Nil)
    assert(sentence.op == A)
    assert(sentence.sentences.length == 5 )
    assert(sentence.sentences.contains(sentence1))
    assert(sentence.sentences.contains(sentence2))
    assert(sentence.sentences.contains(sentence3))
    assert(sentence.sentences.contains(P))
    assert(sentence.sentences.contains(Q))
    
    val symbols = sentence.symbols
    assert(symbols.size == 4)
    assert(symbols.contains(P))
    assert(symbols.contains(Q))
    assert(symbols.contains(R))
    assert(symbols.contains(S))
    
  }
  
  def testHornClause = {
    val P = Symbol("P")
    val Q = Symbol("Q")
    val R = Symbol("R")
    val S = Symbol("S")
    
    val hc = HornClause(P::Q::R::Nil,S)

    assertEquals(hc.premiseSymbols.size,3)
    assertTrue(hc.premiseSymbols.contains(P))
    assertTrue(hc.premiseSymbols.contains(Q))
    assertTrue(hc.premiseSymbols.contains(R))
    assertEquals(hc.head,S)

    assertEquals(hc.symbols.size,4)
    assertTrue(hc.symbols.contains(P))
    assertTrue(hc.symbols.contains(Q))
    assertTrue(hc.symbols.contains(R))
    assertTrue(hc.symbols.contains(S))
    
    
    assertEquals(hc.op,Operator.->)
    
  }
  
  def testFlatten = {
    val P = Symbol("P")
    val Q = Symbol("Q")
    val R = Symbol("R")
    val S = Symbol("S")
    
    val pq = BinarySentence[Symbol,Symbol](A,P,Q)
    val rs = BinarySentence[Symbol,Symbol](A,R,S)
    val pqrs = BinarySentence[Sentence,Sentence](A,pq,rs)
    var pqrsFlat = pqrs.flatten
    assert(pqrsFlat.sentences.size == 4)
    assert(pqrsFlat.op == A)
    assert(pqrsFlat.sentences.contains(P))
    assert(pqrsFlat.sentences.contains(Q))
    assert(pqrsFlat.sentences.contains(R))
    assert(pqrsFlat.sentences.contains(S))
    
    val pqr = MultiSentence[Sentence](A,P::Q::R::Nil)
    val pqrs1 = BinarySentence[Sentence,Symbol](A,pqr,S)
    var pqrs1Flat = pqrs1.flatten
    assert(pqrs1Flat.sentences.size == 4)
    assert(pqrs1Flat.op == A)
    assert(pqrs1Flat.sentences.contains(P))
    assert(pqrs1Flat.sentences.contains(Q))
    assert(pqrs1Flat.sentences.contains(R))
    assert(pqrs1Flat.sentences.contains(S))
    
    
    val pqflat = pq.flatten
    assert(pqflat == pq)
    
    val qr = BinarySentence[Symbol,Symbol](A,Q,R)
    val sp = BinarySentence[Symbol,Symbol](A,S,P)
    
    val allPairs = MultiSentence[Sentence](A,pq::qr::rs::sp::Nil)
	var allPairsFlat = allPairs.flatten
 
    println(allPairsFlat)

    
  }

}
